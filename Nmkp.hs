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
    hName :: String,
    maxHitPoints :: Int,
    hitPoints :: Int,
    attacks :: [Attack]
} deriving (Show)

data Assets = Assets {
    asCow :: Picture,
    asGrass :: Picture,
    asVegan :: Picture,
    asCarnist :: Picture,
    asGameOver :: Picture
}

data Turn = DefenderTurn
          | AttackerTurn
          deriving (Show)

data BattleState = Challenge Human -- "Attacker name" wants to eat you
                 | DefenderAttackChoose
                 | AnnounceAttack Human Attack -- "Human name" uses "attack name"
                 | AnnounceDamage Attack -- "attack name" inflicted "x" damage
                 | DefenderVictor Human -- "Attacker name" has been defeated
                 | AttackerVictor Human -- You have been defeated by "attacker name"!
                 deriving (Show)

data Battle = Battle {
    turn :: Turn,
    defender :: Human,
    attacker :: Human,
    selectedAttack :: Int,
    bState :: BattleState
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

mkHuman :: String -> Int -> [Attack] -> Human
mkHuman name hitPoints attacks = Human {
    hName = name,
    maxHitPoints = hitPoints,
    hitPoints = hitPoints,
    attacks = attacks
}

mkBattle :: Human -> Battle
mkBattle defender = Battle {
    turn = DefenderTurn,
    defender = defender,
    attacker = defaultAttacker,
    selectedAttack = 0,
    bState = Challenge defaultAttacker
}

defaultAttacker :: Human
defaultAttacker = mkHuman "Butcher" 50 [
        Attack {
            attName = "Need my protein",
            damage = 15
        }
    ]

defaultDefender :: Human
defaultDefender = mkHuman "Animal rights activist" 100 [
        Attack {
            attName = "Vegan pizza",
            damage = 5
        },
        Attack {
            attName = "Try Huel", -- required product placement
            damage = 25
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
    assets = Assets {
        asCow = png "./assets/cow.png",
        asGrass = scale 2 2 $ png "./assets/grass.png",
        asVegan = scale 5.075 5.075 $ png "./assets/vegan.png",
        asCarnist = scale 4.7 4.7 $ png "./assets/carnist.png",
        asGameOver = png "./assets/gameover.png"
    },
    defenders = [
        defaultDefender
    ],
    battle = Nothing,
    cow = Cow {
        movement = Done (10, 10)
    },
    stepsSinceBattle = 0,
    grid = Grid {rows = 21, columns = 21, displayRatio = 32}
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
drawAttacker assets Human{..} = translate 189 210 $ asCarnist assets

drawAttackerHP :: Human -> Picture
drawAttackerHP = drawHP (-146, 218)

-- origin point = (0, -235)
-- size = (672, 202)
drawMenuScreen :: Battle -> Picture
drawMenuScreen battle@Battle{..} =
    let itemOffset idx = - 138 - (25 + fromIntegral idx * 48)
        selectedOffset = itemOffset selectedAttack
        textOffsets = map itemOffset ([0..3] :: [Int])
        attacks = getCurrentAttacks battle
        attacksAndOffsets = zip textOffsets attacks
        drawAttack (y, Attack{..}) = color black . translate (-259) y . scale 0.15 0.15 $ text attName
    in pictures $
            map drawAttack attacksAndOffsets ++ [
                -- draw border
                -- draw selection arrow
                color black . translate (-307) selectedOffset $ thickCircle 0 20
            ]

-- origin point = (0, -235)
-- size = (672, 202)
drawMenuScreenWithAnnouncement :: String -> Picture
drawMenuScreenWithAnnouncement msg = color black . translate (-259) (-160) . scale 0.15 0.15 $ text msg

drawFullBattleScreenWithAnnouncement :: Assets -> Battle -> String -> Picture
drawFullBattleScreenWithAnnouncement assets battle msg =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement msg,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleChallenge :: Assets -> Human -> Picture
drawBattleChallenge assets attacker = pictures [
        drawMenuScreenWithAnnouncement (hName attacker ++ " wants to eat you"),
        drawAttacker assets attacker
    ]

drawBattleAttackChoose :: Assets -> Battle -> Picture
drawBattleAttackChoose assets battle =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreen battle,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleAttackAnnounce :: Assets -> Battle -> Human -> Attack -> Picture
drawBattleAttackAnnounce assets battle Human{hName} Attack{attName} = drawFullBattleScreenWithAnnouncement assets battle (hName ++ " uses " ++ attName)

drawBattleDamageAnnounce :: Assets -> Battle -> Attack -> Picture
drawBattleDamageAnnounce assets battle Attack{attName, damage} = drawFullBattleScreenWithAnnouncement assets battle (attName ++ " inflicted " ++ show damage)

drawBattleDefenderVictor :: Assets -> Battle -> Human -> Picture
drawBattleDefenderVictor assets battle Human{hName} =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement (hName ++ " has been defeated"),
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleAttackerVictor :: Assets -> Battle -> Human -> Picture
drawBattleAttackerVictor assets battle Human{hName} =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement ("You have been defeated by " ++ hName ++ "!"),
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleState :: World -> Picture
drawBattleState World{assets, battle = (Just Battle{bState = Challenge attacker})} = drawBattleChallenge assets attacker
drawBattleState World{assets, battle = (Just battle@Battle{bState = DefenderAttackChoose})} = drawBattleAttackChoose assets battle
drawBattleState World{assets, battle = (Just battle@Battle{bState = AnnounceAttack human attack})} = drawBattleAttackAnnounce assets battle human attack
drawBattleState World{assets, battle = (Just battle@Battle{bState = AnnounceDamage attack})} = drawBattleDamageAnnounce assets battle attack
drawBattleState World{assets, battle = (Just battle@Battle{bState = DefenderVictor attacker})} = drawBattleDefenderVictor assets battle attacker
drawBattleState World{assets, battle = (Just battle@Battle{bState = AttackerVictor attacker})} = drawBattleAttackerVictor assets battle attacker
drawBattleState World{battle = Nothing} = undefined

drawGameOver :: World -> Picture
drawGameOver World{assets} = asGameOver assets

draw :: World -> Picture
draw world@World{state = InField} = drawFieldState world
draw world@World{state = InBattle} = drawBattleState world
draw world@World{state = GameOver} = drawGameOver world

handleMoveInput :: Event -> World -> World
handleMoveInput (EventKey (Char 'w') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Up cow}
handleMoveInput (EventKey (Char 's') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Down cow}
handleMoveInput (EventKey (Char 'a') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Left cow}
handleMoveInput (EventKey (Char 'd') GlossKey.Down _ _) world@World{cow} = world {cow = moveCowIfNotMoving Right cow}
handleMoveInput _ world = world

handleBattleChallengeInput :: Event -> World -> World
handleBattleChallengeInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle} = world {battle = Just $ battle {bState = DefenderAttackChoose}}
handleBattleChallengeInput _ world = world

handleBattleSelectionInput :: Event -> World -> World
handleBattleSelectionInput (EventKey (Char 'w') GlossKey.Up _ _) world@World{battle = (Just battle)} = world {battle = Just $ battle {selectedAttack = selectedAttack battle - 1}}
handleBattleSelectionInput (EventKey (Char 's') GlossKey.Up _ _) world@World{battle = (Just battle)} = world {battle = Just $ battle {selectedAttack = selectedAttack battle + 1}}
handleBattleSelectionInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = (Just battle)} =
    world {battle = Just $ battle {bState = AnnounceAttack (defender battle) (getSelectedAttack battle)}}
handleBattleSelectionInput _ world = world

handleBattleAttackAnnounce :: Event -> World -> World
handleBattleAttackAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle@Battle{bState = AnnounceAttack _ attack}} =
    world {battle = Just $ battle {bState = AnnounceDamage attack}}
handleBattleAttackAnnounce _ world = world

handleBattleDamageAnnounce :: Event -> World -> World
handleBattleDamageAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle@Battle{bState = AnnounceDamage attack, turn = DefenderTurn}} =
    let battle' = applyAttackToBattle attack battle
        attacker' = attacker battle'
        won = hitPoints attacker' == 0
    in if won
        then world {battle = Just $ battle' {bState = DefenderVictor (defender battle')}}
        else world {battle = Just . swapTurn $ battle' {bState = AnnounceAttack attacker' (head $ attacks attacker')}}
handleBattleDamageAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle@Battle{bState = AnnounceDamage attack, turn = AttackerTurn}} =
    let battle' = applyAttackToBattle attack battle
        won = hitPoints (defender battle') == 0
    in if won
        then world {battle = Just $ battle' {bState = AttackerVictor (attacker battle')}}
        else world {battle = Just . swapTurn $ battle' {bState = DefenderAttackChoose}}
handleBattleDamageAnnounce _ world = world

handleBattleDefenderVictor :: Event -> World -> World
handleBattleDefenderVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world =
    world {state = InField, battle = Nothing}
handleBattleDefenderVictor _ world = world

handleBattleAttackerVictor :: Event -> World -> World
handleBattleAttackerVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world =
    world {state = GameOver, battle = Nothing}
handleBattleAttackerVictor _ world = world

handleBattleInput :: Event -> World -> World
handleBattleInput event world@World{battle = (Just Battle{bState = Challenge _})} = handleBattleChallengeInput event world
handleBattleInput event world@World{battle = (Just Battle{bState = DefenderAttackChoose})} = handleBattleSelectionInput event world
handleBattleInput event world@World{battle = (Just Battle{bState = AnnounceAttack _ _})} = handleBattleAttackAnnounce event world
handleBattleInput event world@World{battle = (Just Battle{bState = AnnounceDamage attack})} = handleBattleDamageAnnounce event world
handleBattleInput event world@World{battle = (Just Battle{bState = DefenderVictor defender})} = handleBattleDefenderVictor event world
handleBattleInput event world@World{battle = (Just Battle{bState = AttackerVictor attacker})} = handleBattleAttackerVictor event world

handleInput :: Event -> World -> World
handleInput event world@World{state = InField} = handleMoveInput event world
handleInput event world@World{state = InBattle} = handleBattleInput event world
handleInput _ world@World{state = GameOver} = world

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
                        battle = Just . mkBattle $ head defenders
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
