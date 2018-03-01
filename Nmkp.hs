{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Graphics.Gloss.Game hiding (Up, Down)
import qualified Graphics.Gloss.Game as GlossKey
import Prelude hiding (Left, Right)

data Grid = Grid {
    rows :: Int,
    columns :: Int,
    displayRatio :: Int
} deriving (Show)

type GridPos = (Int, Int)
type DisplayPos = (Float, Float)

data Direction = Down
               | Up
               | Left
               | Right
               deriving (Show)

data Move = Start Direction GridPos GridPos
          | Moving Direction Int GridPos GridPos
          | Done GridPos
          deriving (Show)

data Cow = Cow {
    movement :: Move
} deriving (Show)

data Attack = Attack {
    attName :: String,
    damage :: Int
} deriving (Show)

data Human = Human {
    maxHitPoints :: Int,
    hitPoints :: Int,
    attacks :: [Attack]
} deriving (Show)

data Assets = Assets {
    asCow :: Picture,
    asGrass :: Picture,
    asVegan :: Picture,
    asCarnist :: Picture
}

data Turn = DefenderTurn 
          | AttackerTurn
          deriving (Show)

data Battle = Battle {
    turn :: Turn,
    defender :: Human,
    attacker :: Human,
    selectedAttack :: Int
} deriving (Show)

data WorldState = InField
                | InBattle
                | GameOver
                deriving (Show)

data World = World {
    state :: WorldState,
    grid :: Grid,
    cow :: Cow,
    defenders :: [Human],
    battle :: Maybe Battle,
    stepsSinceBattle :: Int,
    assets :: Assets
}

stepsUntilBattle :: Int
stepsUntilBattle = 2

mkHuman :: Int -> [Attack] -> Human
mkHuman hitPoints attacks = Human {
    maxHitPoints = hitPoints,
    hitPoints = hitPoints,
    attacks = attacks
}

mkBattle :: Human -> Battle
mkBattle defender = Battle {
    turn = DefenderTurn,
    defender = defender,
    attacker = defaultAttacker,
    selectedAttack = 0
}

defaultAttacker :: Human
defaultAttacker = mkHuman 50 [
        Attack {
            attName = "Need mah protein",
            damage = 5
        }
    ]

defaultDefender :: Human
defaultDefender = mkHuman 100 [
        Attack {
            attName = "Vegan proganda!",
            damage = 10
        },
        Attack {
            attName = "Try Huel",
            damage = 15
        },
        Attack {
            attName = "Carnism is not sustainable",
            damage = 15
        },
        Attack {
            attName = "Hippie outlook on life",
            damage = 15
        }
    ]

getCurrentAttacks :: Battle -> [Attack]
getCurrentAttacks Battle{turn = DefenderTurn, defender} = attacks defender
getCurrentAttacks Battle{turn = AttackerTurn, attacker} = attacks attacker

getSelectedAttack :: Battle -> Attack
getSelectedAttack Battle{turn = DefenderTurn, defender = Human{attacks}, selectedAttack} = attacks !! selectedAttack
getSelectedAttack Battle{turn = AttackerTurn, attacker = Human{attacks}, selectedAttack} = attacks !! selectedAttack

applyAttack :: Attack -> Human -> Human
applyAttack Attack{damage} human@Human{hitPoints} =
    let uncheckedHitPoints = hitPoints - damage
        hitPoints' = max 0 uncheckedHitPoints
    in human{hitPoints = hitPoints'}

applyAttackToBattle :: Attack -> Battle -> Battle
applyAttackToBattle attack battle@Battle{turn = DefenderTurn, attacker} = battle{attacker = applyAttack attack attacker}
applyAttackToBattle attack battle@Battle{turn = AttackerTurn, defender} = battle{defender = applyAttack attack defender}

swapTurn :: Battle -> Battle
swapTurn battle@Battle{turn = DefenderTurn} = battle{turn = AttackerTurn, selectedAttack = 0}
swapTurn battle@Battle{turn = AttackerTurn} = battle{turn = DefenderTurn, selectedAttack = 0}

playAttacker :: Battle -> Battle
playAttacker battle =
    let battleAttackerTurn = swapTurn battle
        attack = getSelectedAttack battleAttackerTurn
        battle' = applyAttackToBattle attack battleAttackerTurn
    in swapTurn battle'

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
getDisplayPosFromMovement grid                    (Start _ start _) = gridToDisplayPos grid start
getDisplayPosFromMovement grid                    (Done end) = gridToDisplayPos grid end
getDisplayPosFromMovement grid@Grid{displayRatio} (Moving direction iteration start _) =
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
        asGrass = scale 2 2 $ png "./assets/grass.png",
        asVegan = scale 5.075 5.075 $ png "./assets/vegan.png",
        asCarnist = scale 4.7 4.7 $ png "./assets/carnist.png"
    }),
    defenders = [
        defaultDefender
    ],
    battle = Nothing,
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

drawFieldState :: World -> Picture
drawFieldState World{..} = pictures (grassBackground (asGrass assets) grid ++ [
        translate cowX cowY (asCow assets)
    ])
    where
        cowMove = movement cow
        (cowX, cowY) = getDisplayPosFromMovement grid cowMove

drawHP :: (Float, Float) -> Human -> Picture
drawHP (x, y) Human{..} =
    let percentageLeft = fromIntegral hitPoints / fromIntegral maxHitPoints
        maxHitPointBarLength = 202
        hitPointBarLength = maxHitPointBarLength * percentageLeft
    in pictures [
        color (greyN 0.55) . translate x y $ rectangleSolid 210 18, -- Background (100%)
        color (greyN 0.85) . translate x y $ rectangleSolid hitPointBarLength 10 -- Actual hitpoints
    ]

drawDefender :: Assets -> Human -> Picture
drawDefender assets Human{..} = translate (-189) 8 $ asVegan assets

drawDefenderHP :: Human -> Picture
drawDefenderHP = drawHP (191, -34)

drawAttacker :: Assets -> Human -> Picture
drawAttacker assets Human{..} = translate (189) 210 $ asCarnist assets

drawAttackerHP :: Human -> Picture
drawAttackerHP = drawHP (-146, 218)

-- origin point = (0, -235)
-- size = (672, 202)
drawMenuScreen :: Battle -> Picture
drawMenuScreen battle@Battle{..} =
    let itemOffset idx = - 138 - (25 + fromIntegral idx * 48)
        selectedOffset = itemOffset selectedAttack
        textOffsets = map itemOffset [0..3]
        attacks = getCurrentAttacks battle
        attacksAndOffsets = zip textOffsets attacks
        drawAttack (y, Attack{..}) = color black . translate (-259) y . scale 0.15 0.15 $ text attName
    in pictures $
            map drawAttack attacksAndOffsets ++ [
                -- draw border
                -- draw selection arrow
                color black . translate (-307) selectedOffset $ thickCircle 0 20
            ]

drawBattleState :: World -> Picture
drawBattleState World{assets, battle = (Just battle)} =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreen battle,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]
drawBattleState World{battle = Nothing} = undefined

draw :: World -> Picture
draw world@World{state = InField} = drawFieldState world
draw world@World{state = InBattle} = drawBattleState world

handleMoveInput :: Event -> World -> World
handleMoveInput (EventKey (Char 'w') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Up cow}
handleMoveInput (EventKey (Char 's') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Down cow}
handleMoveInput (EventKey (Char 'a') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Left cow}
handleMoveInput (EventKey (Char 'd') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Right cow}
handleMoveInput _ world = world

handleBattleInput :: Event -> World -> World
handleBattleInput (EventKey (Char 'w') GlossKey.Up _ _) world@World{battle = (Just battle)} = world {battle = Just $ battle {selectedAttack = selectedAttack battle - 1}}
handleBattleInput (EventKey (Char 's') GlossKey.Up _ _) world@World{battle = (Just battle)} = world {battle = Just $ battle {selectedAttack = selectedAttack battle + 1}}
handleBattleInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = (Just battle)} =
    let attack = getSelectedAttack battle
        battle' = applyAttackToBattle attack battle
        won = (hitPoints $ attacker battle') == 0
    in if won
        then world {
                battle = Nothing,
                state = InField
            }
        else let battlePostAttacker = playAttacker battle'
                 gameOver = (hitPoints $ defender battlePostAttacker) == 0
             in if gameOver
                then world {battle = Nothing, state = GameOver}
                else world {battle = Just $ battlePostAttacker}
handleBattleInput _ world = world


handleInput :: Event -> World -> World
handleInput event world@World{state = InField} = handleMoveInput event world
handleInput event world@World{state = InBattle} = handleBattleInput event world

updateMovePosition :: World -> World
updateMovePosition world@World{cow, stepsSinceBattle, defenders} =
    case movement cow of
        Start direction start end            -> world {cow = cow {movement = Moving direction 1 start end}}
        Moving _ 15 _ end                    ->
            let stepsSinceBattle' = stepsSinceBattle + 1
                underAttack = stepsSinceBattle' == stepsUntilBattle
                worldWithUpdatedCow = world {cow = cow {movement = Done end}}
            in if underAttack
                then worldWithUpdatedCow {
                        stepsSinceBattle = 0,
                        state = InBattle,
                        battle = Just $ mkBattle (defenders !! 0)
                    }
                else worldWithUpdatedCow {
                        stepsSinceBattle = stepsSinceBattle'
                    }
        Moving direction iteration start end -> world {cow = cow {movement = Moving direction (iteration + 1) start end}}
        Done _                               -> world

updateFieldPosition :: Float -> World -> World
updateFieldPosition _ world@World{state = InField} = updateMovePosition world
updateFieldPosition _ world                        = world

windowHeight :: Grid -> Int
windowHeight Grid{..} = rows * displayRatio

windowWidth :: Grid -> Int
windowWidth Grid{..} = columns * displayRatio

main :: IO ()
main = do
    let World{grid} = initialWorld
    play (InWindow "Nmkp" (windowWidth grid, windowHeight grid) (0, 0))
         white
         30
         initialWorld
         draw
         handleInput
         [updateFieldPosition]
