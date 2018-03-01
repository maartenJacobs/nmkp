{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Graphics.Gloss.Game hiding (Up, Down, Left, Right)
import qualified Graphics.Gloss.Game as GlossKey
import Prelude hiding (Left, Right)

data Grid = Grid {
    rows :: Int,
    columns :: Int,
    displayRatio :: Int
}

type GridPos = (Int, Int)
type DisplayPos = (Float, Float)

data Direction = Down
               | Up
               | Left
               | Right

data Move = Start Direction GridPos GridPos
          | Moving Direction Int GridPos GridPos
          | Done GridPos

data Cow = Cow {
    movement :: Move
}

data Assets = Assets {
    asCow :: Picture,
    asGrass :: Picture
}

data WorldState = InField | InBattle

data World = World {
    state :: WorldState,
    grid :: Grid,
    cow :: Cow,
    stepsSinceBattle :: Int,
    assets :: Assets
}

isMovementDone :: Move -> Bool
isMovementDone (Done _) = True
isMovementDone _        = False

getGridPosFromMove :: Move -> GridPos
getGridPosFromMove (Start _ pos _) = pos
getGridPosFromMove (Moving _ _ pos _) = pos
getGridPosFromMove (Done pos) = pos

getNextGridPos :: Move -> Direction -> GridPos
getNextGridPos movement direction =
    applyOffsetFromDirection direction 1 (getGridPosFromMove movement)

applyOffsetFromDirection :: Num a => Direction -> a -> (a, a) -> (a, a)
applyOffsetFromDirection Up offset (x, y) = (x, y - offset)
applyOffsetFromDirection Down offset (x, y) = (x, y + offset)
applyOffsetFromDirection Left offset (x, y) = (x - offset, y)
applyOffsetFromDirection Right offset (x, y) = (x + offset, y)

getDisplayPosFromMovement :: Grid -> Move -> DisplayPos
getDisplayPosFromMovement grid                    (Start direction start _) = gridToDisplayPos grid start
getDisplayPosFromMovement grid                    (Done end) = gridToDisplayPos grid end
getDisplayPosFromMovement grid@Grid{displayRatio} (Moving direction iteration start end) =
    let gridPositionInDisplay = gridToDisplayPos grid start
        offset = (fromIntegral displayRatio :: Float) / 15 * fromIntegral iteration
        displayOffset = case direction of
            Up -> -offset
            Down -> -offset
            _ -> offset
    in applyOffsetFromDirection direction displayOffset gridPositionInDisplay

gridToDisplayPos :: Grid -> GridPos -> DisplayPos
gridToDisplayPos Grid{..} (x, y) = (fromIntegral x', fromIntegral y')
    where
        x' = (-displayRatio * columns `div` 2) + x * displayRatio + displayRatio `div` 2
        y' = (displayRatio * rows `div` 2) - y * displayRatio - displayRatio `div` 2

moveCowIfNotMoving :: Direction -> Cow -> Cow
moveCowIfNotMoving direction cow
    | isMovementDone cowMove = cow {
            movement = Start direction (getGridPosFromMove cowMove) (getNextGridPos cowMove direction)
        }
    | otherwise = cow
    where
        cowMove = movement cow

initialWorld :: World
initialWorld = World {
    state = InField,
    assets = (Assets {
        asCow = png "./assets/cow.png",
        asGrass = (scale 2 2 $ png "./assets/grass.png")
    }),
    cow = (Cow {
        movement = Done (10, 10)
    }),
    stepsSinceBattle = 0,
    grid = Grid {rows=21, columns=21, displayRatio=32}
}

grassBackground :: Picture -> Grid -> [Picture]
grassBackground grass grid@Grid{..} =
    let gridPositions = [(x, y) | x <- [0..(columns - 1)], y <- [0..(rows - 1)]]
        displayPositions = map (gridToDisplayPos grid) gridPositions
        positionToTranslation (x, y) = translate x y grass
    in map positionToTranslation displayPositions

draw :: World -> Picture
draw World{..} = pictures (grassBackground (asGrass assets) grid ++ [
        translate cowX cowY (asCow assets)
    ])
    where
        cowMove = movement cow
        (cowX, cowY) = getDisplayPosFromMovement grid cowMove

handleMoveInput :: Event -> World -> World
handleMoveInput (EventKey (Char 'w') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Up cow}
handleMoveInput (EventKey (Char 's') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Down cow}
handleMoveInput (EventKey (Char 'a') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Left cow}
handleMoveInput (EventKey (Char 'd') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Right cow}
handleMoveInput _ world = world

handleInput :: Event -> World -> World
handleInput event world@World{state=InField} = handleMoveInput event world
handleInput _ world = world

updateCowPosition :: Float -> World -> World
updateCowPosition _ world@World{cow, stepsSinceBattle} =
    case movement cow of
        Start direction start end            -> world {cow = cow {movement = Moving direction 1 start end}}
        Moving _ 15 _ end                    ->
            let stepsSinceBattle' = stepsSinceBattle + 1
                underAttack = stepsSinceBattle' == 3
                state' = if underAttack then InBattle else InField
            in world {
                    cow = cow {movement = Done end},
                    stepsSinceBattle = stepsSinceBattle',
                    state = state'
                }
        Moving direction iteration start end -> world {cow = cow {movement = Moving direction (iteration + 1) start end}}
        Done _                               -> world

main :: IO ()
main = play (InWindow "Nmkp" (windowWidth, windowHeight) (0, 0))
            white
            30
            initialWorld
            draw
            handleInput
            [updateCowPosition]
    where
        World{grid=Grid{..}} = initialWorld
        windowWidth = rows * displayRatio
        windowHeight = columns * displayRatio
