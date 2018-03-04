{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}

module Nmkp (playNmkp) where

import Graphics.Gloss.Game hiding (Up, Down)
import qualified Graphics.Gloss.Game as GlossKey
import Prelude hiding (Left, Right)
import System.Random
import Data.Tuple (swap)

data Grid = Grid {
    rows :: Int,
    columns :: Int,
    displayRatio :: Int
} deriving (Show)

type GridPos = (Int, Int)

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
    movement :: Move,
    cowCurrentAsset :: Assets -> Picture,
    cowAssets :: (Assets -> Picture, Assets -> Picture),
    cowImageFlip :: Int
}

data Attack = Attack {
    attName :: String,
    damage :: Int
}

data Human = Human {
    hName :: String,
    maxHitPoints :: Int,
    hitPoints :: Int,
    hGetAsset :: Assets -> Picture,
    attacks :: [Attack],
    convertsTo :: Maybe Human
}

data Assets = Assets {
    asCow :: Picture,
    asCowHeadDown :: Picture,
    asGrass :: Picture,
    asVegan :: Picture,
    asCarnist :: Picture,
    asDairyFarmer :: Picture,
    asObeseMan :: Picture,
    asFatVegan :: Picture,
    asNutMilker :: Picture,
    asGameOver :: Picture
}

data Turn = DefenderTurn
          | AttackerTurn
          deriving (Show)

data BattleMenuOption = ContinueBattle
                      | ConvertAttacker
                      | ChooseDefender

instance Show BattleMenuOption where
    show :: BattleMenuOption -> String
    show ContinueBattle = "Fight"
    show ConvertAttacker = "Convert"
    show ChooseDefender = "Defenders"

data ConvertState = ConvertAnnounceAttempt
                  | ConvertAnnouncePokeball
                  | ConvertAnnounceSuccess Human -- New defender
                  | ConvertAnnounceFailure

data BattleState = Challenge Human -- "Attacker name" wants to eat you
                 | DefenderAttackChoose
                 | AnnounceAttack Human Attack -- "Human name" uses "attack name"
                 | AnnounceDamage Attack -- "attack name" inflicted "x" damage
                 | DefenderVictor Human -- "Attacker name" has been defeated
                 | AnnounceDefenderDefeated Human -- "Defender name" has been defeated. Bringing in the next defender.
                 | AttackerVictor Human -- You have been defeated by "attacker name"!
                 | BattleMenuOpen [BattleMenuOption] Int
                 | Convert ConvertState
                 | ChooseDefenderMenuOpen [Human] Int

data Battle = Battle {
    turn :: Turn,
    bDefenders :: [Human],
    bSelectedDefender :: Int,
    attacker :: Human,
    selectedAttack :: Int,
    bState :: BattleState
}

data WorldState = InField
                | InBattle
                | GameOver
                deriving (Show)

data World = World {
    state :: WorldState,
    gen :: StdGen,
    grid :: Grid,
    cow :: Cow,
    defenders :: [Human],
    battle :: Maybe Battle,
    stepsSinceBattle :: Int,
    assets :: Assets
}

mkHuman :: String -> Int -> (Assets -> Picture) -> Maybe Human -> [Attack] -> Human
mkHuman name hitPoints getAsset convertsTo attacks = Human {
    hName = name,
    maxHitPoints = hitPoints,
    hitPoints = hitPoints,
    hGetAsset = getAsset,
    attacks = attacks,
    convertsTo = convertsTo
}

mkDefender :: String -> Int -> (Assets -> Picture) -> [Attack] -> Human
mkDefender name hitPoints getAsset = mkHuman name hitPoints getAsset Nothing

mkAttacker :: String -> Int -> (Assets -> Picture) -> Human -> [Attack] -> Human
mkAttacker name hitPoints getAsset convertsTo = mkHuman name hitPoints getAsset (Just convertsTo)

mkBattle :: [Human] -> Human -> Battle
mkBattle defenders attacker = Battle {
    turn = DefenderTurn,
    bDefenders = defenders,
    bSelectedDefender = 0,
    attacker = attacker,
    selectedAttack = 0,
    bState = Challenge attacker
}

animalRightsActivist :: Human
animalRightsActivist = defaultDefender

hungryCarnist :: Human
hungryCarnist = mkAttacker "Hungry carnist" 50 asCarnist animalRightsActivist [
        Attack {
            attName = "Plants have feelings",
            damage = 15
        },
        Attack {
            attName = "Facon isn't bacon",
            damage = 5
        },
        Attack {
            attName = "How can I get my protein?",
            damage = 10
        }
    ]

fatVegan :: Human
fatVegan = mkDefender "Fat vegan" 100 asFatVegan [
        Attack {
            attName = "Vegan meals are so cheap",
            damage = 10
        },
        Attack {
            attName = "Facon 4 lyfe",
            damage = 10
        }
    ]

obeseMan :: Human
obeseMan = mkAttacker "Obese man" 100 asObeseMan fatVegan [
        Attack {
            attName = "I need 15 meals a day",
            damage = 10
        },
        Attack {
            attName = "Vegan food is more expensive",
            damage = 5
        },
        Attack {
            attName = "Vegetables are tasteless",
            damage = 10
        }
    ]

nutMilker :: Human
nutMilker = mkDefender "Nut milker" 55 asNutMilker [
        Attack {
            attName = "Grind nuts",
            damage = 5
        },
        Attack {
            attName = "Spray mylk",
            damage = 8
        }
    ]

dairyFarmer :: Human
dairyFarmer = mkAttacker "Dairy farmer" 50 asDairyFarmer nutMilker [
        Attack {
            attName = "Nut milk tastes terrible",
            damage = 15
        },
        Attack {
            attName = "Try milking tiny almond nipples",
            damage = 5
        },
        Attack {
            attName = "Soy killed me wife",
            damage = 10
        }
    ]

defaultAttackers :: [Human]
defaultAttackers = [
        hungryCarnist,
        dairyFarmer,
        obeseMan
    ]

defaultDefender :: Human
defaultDefender = mkDefender "Animal rights activist" 100 asVegan [
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

replaceAt :: [a] -> Int -> a -> [a]
replaceAt as idx a =
    case splitAt idx as of
        (pre, _ : post) -> pre ++ (a : post)
        (_, []) -> undefined -- Should not be possible?

getSelectedDefender :: Battle -> Human
getSelectedDefender Battle{bDefenders, bSelectedDefender} = bDefenders !! bSelectedDefender

removeSelectedDefender :: Battle -> [Human]
removeSelectedDefender Battle{bDefenders, bSelectedDefender} =
    let (pre, _ : post) = splitAt bSelectedDefender bDefenders
    in pre ++ post

getSelectedDefenderAttacks :: Battle -> [Attack]
getSelectedDefenderAttacks Battle{bDefenders, bSelectedDefender} = attacks (bDefenders !! bSelectedDefender)

updateSelectedDefender :: Battle -> (Human -> Human) -> Battle
updateSelectedDefender battle@Battle{bDefenders, bSelectedDefender} update =
    let selectedDefender' = update (bDefenders !! bSelectedDefender)
    in battle{bDefenders = replaceAt bDefenders bSelectedDefender selectedDefender'}

getCurrentAttacks :: Battle -> [Attack]
getCurrentAttacks battle@Battle{turn = DefenderTurn} = attacks (getSelectedDefender battle)
getCurrentAttacks Battle{turn = AttackerTurn, attacker} = attacks attacker

getSelectedAttack :: Battle -> Attack
getSelectedAttack battle@Battle{turn = DefenderTurn, selectedAttack} = getSelectedDefenderAttacks battle !! selectedAttack
getSelectedAttack Battle{turn = AttackerTurn, attacker = Human{attacks}, selectedAttack} = attacks !! selectedAttack

applyAttack :: Attack -> Human -> Human
applyAttack Attack{damage} human@Human{hitPoints} =
    let uncheckedHitPoints = hitPoints - damage
        hitPoints' = max 0 uncheckedHitPoints
    in human{hitPoints = hitPoints'}

applyAttackToBattle :: Attack -> Battle -> Battle
applyAttackToBattle attack battle@Battle{turn = DefenderTurn, attacker} = battle{attacker = applyAttack attack attacker}
applyAttackToBattle attack battle@Battle{turn = AttackerTurn} = updateSelectedDefender battle (applyAttack attack)

swapTurn :: Battle -> Battle
swapTurn battle@Battle{turn = DefenderTurn} = battle{turn = AttackerTurn, selectedAttack = 0}
swapTurn battle@Battle{turn = AttackerTurn} = battle{turn = DefenderTurn, selectedAttack = 0}

swapDefender :: Int -> Battle -> Battle
swapDefender selectedIdx battle = battle {bSelectedDefender = selectedIdx}

isMovementDone :: Move -> Bool
isMovementDone (Done _) = True
isMovementDone _        = False

getGridPosFromMove :: Move -> GridPos
getGridPosFromMove (Start _ pos _) = pos
getGridPosFromMove (Moving _ _ pos _) = pos
getGridPosFromMove (Done pos) = pos

atEdgeOfGrid :: Grid -> Move -> Bool
atEdgeOfGrid Grid{rows, columns} movement =
    let (x, y) = getGridPosFromMove movement
    in x == 0 || y == 0 || x == (columns - 1) || y == (rows - 1)

getNextGridPos :: Move -> Direction -> GridPos
getNextGridPos movement direction =
    applyOffsetFromDirection direction 1 (getGridPosFromMove movement)

applyOffsetFromDirection :: Num a => Direction -> a -> (a, a) -> (a, a)
applyOffsetFromDirection Up offset (x, y) = (x, y - offset)
applyOffsetFromDirection Down offset (x, y) = (x, y + offset)
applyOffsetFromDirection Left offset (x, y) = (x - offset, y)
applyOffsetFromDirection Right offset (x, y) = (x + offset, y)

getDisplayPosFromMovement :: Grid -> Move -> Point
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

gridToDisplayPos :: Grid -> GridPos -> Point
gridToDisplayPos Grid{..} (x, y) = (fromIntegral x', fromIntegral y')
    where
        x' = (-displayRatio * columns `div` 2) + x * displayRatio + displayRatio `div` 2
        y' = (displayRatio * rows `div` 2) - y * displayRatio - displayRatio `div` 2

moveCowIfNotMoving :: Grid -> Cow -> Direction -> Cow
moveCowIfNotMoving grid cow direction
    | isMovementDone cowMove && not (atEdgeOfGrid grid cowMove) = cow {
            movement = Start direction (getGridPosFromMove cowMove) (getNextGridPos cowMove direction)
        }
    | otherwise = cow
    where
        cowMove = movement cow

chanceOfAttack :: Int -> Int
chanceOfAttack 1 = 12
chanceOfAttack 2 = 15
chanceOfAttack 3 = 20
chanceOfAttack 4 = 30
chanceOfAttack 5 = 45
chanceOfAttack 6 = 60
chanceOfAttack 7 = 75
chanceOfAttack 8 = 90
chanceOfAttack 9 = 95
chanceOfAttack _ = 100

isUnderAttack :: StdGen -> Int -> (Bool, StdGen)
isUnderAttack gen stepsSinceBattle =
    let chance = chanceOfAttack stepsSinceBattle
        (luck, gen') = randomR (0, 100) gen
        underAttack = luck <= chance
    in (underAttack, gen')

getRandomListItem :: StdGen -> [a] -> (a, StdGen)
getRandomListItem gen as =
    let (index, gen') = randomR (0, length as - 1) gen
    in (as !! index, gen')

getRandomAttack :: StdGen -> Human -> (Attack, StdGen)
getRandomAttack gen Human{attacks} = getRandomListItem gen attacks

getRandomAttacker :: StdGen -> (Human, StdGen)
getRandomAttacker gen = getRandomListItem gen defaultAttackers

chanceOfCapture :: Float -> Int
chanceOfCapture percentageHPLeft
    | percentageHPLeft <= 10 = 75
    | percentageHPLeft <= 25 = 50
    | percentageHPLeft <= 50 = 35
    | percentageHPLeft <= 85 = 10
    | otherwise = 0

isCaptured :: StdGen -> Human -> (Bool, StdGen)
isCaptured gen Human{hitPoints, maxHitPoints} =
    let percentageHPLeft = (fromIntegral hitPoints / fromIntegral maxHitPoints) * 100
        chance = chanceOfCapture percentageHPLeft
        (luck, gen') = randomR (0, 100) gen
        captured = luck <= chance
    in (captured, gen')

genesis :: StdGen -> World
genesis gen = World {
    state = InField,
    gen = gen,
    assets = Assets {
        asCow = png "./assets/cow.png",
        asCowHeadDown = png "./assets/cow-head-down.png",
        asGrass = scale 2 2 $ png "./assets/grass.png",
        asVegan = scale 5.075 5.075 $ png "./assets/vegan.png",
        asCarnist = scale 4.7 4.7 $ png "./assets/carnist.png",
        asGameOver = png "./assets/gameover.png",
        asDairyFarmer = scale 4.7 4.7 $ png "./assets/dairy-farmer.png",
        asObeseMan = scale 4.7 4.7 $ png "./assets/obese-man.png",
        asFatVegan = scale 4.7 4.7 $ png "./assets/fat-vegan.png",
        asNutMilker = scale 4.7 4.7 $ png "./assets/nut-milker.png"
    },
    defenders = [defaultDefender],
    battle = Nothing,
    cow = Cow {
        movement = Done (10, 10),
        cowCurrentAsset = asCow,
        cowAssets = (asCowHeadDown, asCow),
        cowImageFlip = 0
    },
    stepsSinceBattle = 0,
    grid = Grid {rows = 21, columns = 21, displayRatio = 32}
}

backToField :: Battle -> World -> World
backToField Battle{bDefenders} world = world {defenders = bDefenders, state = InField, battle = Nothing}

addDefender :: Human -> World -> World
addDefender defender world@World{defenders} = world {defenders = defenders ++ [defender]}

grassBackground :: Picture -> Grid -> [Picture]
grassBackground grass grid@Grid{..} =
    let gridPositions = [(x, y) | x <- [0..(columns - 1)], y <- [0..(rows - 1)]]
        displayPositions = map (gridToDisplayPos grid) gridPositions
        positionToTranslation (x, y) = translate x y grass
    in map positionToTranslation displayPositions

drawFieldState :: World -> Picture
drawFieldState World{..} =
    let cowMove = movement cow
        (cowX, cowY) = getDisplayPosFromMovement grid cowMove
        cowAsset = cowCurrentAsset cow
    in pictures $
        grassBackground (asGrass assets) grid ++ [
            translate cowX cowY (cowAsset assets)
        ]

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
drawDefender assets Human{..} = translate (-189) 8 $ hGetAsset assets

drawDefenderHP :: Human -> Picture
drawDefenderHP = drawHP (191, -34)

drawAttacker :: Assets -> Human -> Picture
drawAttacker assets Human{..} = translate 189 210 $ hGetAsset assets

drawAttackerHP :: Human -> Picture
drawAttackerHP = drawHP (-146, 218)

-- origin point = (0, -235)
-- size = (672, 202)
drawMenuScreen :: Battle -> Picture
drawMenuScreen battle@Battle{..} =
    let itemOffset idx = - 170 - (25 + fromIntegral idx * 32)
        selectedOffset = 10 + itemOffset selectedAttack
        textOffsets = map itemOffset ([0..3] :: [Int])
        attacks = getCurrentAttacks battle
        attacksAndOffsets = zip textOffsets attacks
        drawAttack (y, Attack{..}) = color black . translate (-259) y . scale 0.2 0.2 $ text attName
    in pictures $
            map drawAttack attacksAndOffsets ++ [
                color black $ line [(-336, -134), (336, -134)],
                color black . translate (-307) selectedOffset $ thickCircle 0 20
            ]

-- origin point = (0, -235)
-- size = (672, 202)
drawMenuScreenWithAnnouncement :: [String] -> Picture
drawMenuScreenWithAnnouncement msgs =
    let prepareMsg y = color black . translate (-300) y . scale 0.2 0.2 . text
        msgsWithIndex = zip [0..] msgs
        translatedMsgs = map (\(idx, msg) -> prepareMsg (-180 - idx * 40) msg) msgsWithIndex
    in pictures (color black (line [(-336, -134), (336, -134)]) : translatedMsgs)

drawMenuScreenWithBattleMenuLabels :: [String] -> Int -> Picture
drawMenuScreenWithBattleMenuLabels labels selectedIdx =
    let prepareLabel y = color black . translate 152 y . scale 0.2 0.2 . text
        labelsWithIndex = zip [0..] labels
        translatedLabels = map (\(idx, label) -> prepareLabel (-180 - idx * 40) label) labelsWithIndex
        selectedOffset = fromIntegral (-170 - selectedIdx * 40)
    in pictures $
        translatedLabels ++ [
            color black $ line [(-336, -134), (336, -134)],
            color black $ line [(112, -134), (112, -336)],
            color black . translate 130 selectedOffset $ thickCircle 0 20
        ]

drawMenuScreenWithBattleMenu :: [BattleMenuOption] -> Int -> Picture
drawMenuScreenWithBattleMenu options = drawMenuScreenWithBattleMenuLabels (map show options)

drawMenuScreenWithDefendersMenu :: [Human] -> Int -> Picture
drawMenuScreenWithDefendersMenu options = drawMenuScreenWithBattleMenuLabels (map hName options)

drawFullBattleScreenWithAnnouncement :: Assets -> Battle -> [String] -> Picture
drawFullBattleScreenWithAnnouncement assets battle msgs =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement msgs,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleChallenge :: Assets -> Human -> Picture
drawBattleChallenge assets attacker = pictures [
        drawMenuScreenWithAnnouncement [hName attacker ++ " wants to eat you"],
        drawAttacker assets attacker
    ]

drawBattleAttackChoose :: Assets -> Battle -> Picture
drawBattleAttackChoose assets battle =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreen battle,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleAttackAnnounce :: Assets -> Battle -> Human -> Attack -> Picture
drawBattleAttackAnnounce assets battle Human{hName} Attack{attName} = drawFullBattleScreenWithAnnouncement assets battle [hName ++ " used", "  " ++ attName]

drawBattleDamageAnnounce :: Assets -> Battle -> Attack -> Picture
drawBattleDamageAnnounce assets battle Attack{attName, damage} = drawFullBattleScreenWithAnnouncement assets battle [attName, "  inflicted " ++ show damage]

drawBattleDefenderVictor :: Assets -> Battle -> Human -> Picture
drawBattleDefenderVictor assets battle Human{hName} =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement [hName ++ " has been defeated"],
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleAttackerVictor :: Assets -> Battle -> Human -> Picture
drawBattleAttackerVictor assets battle Human{hName} =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement ["You have been defeated by ", hName ++ "!"],
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleMenu :: Assets -> Battle -> [BattleMenuOption] -> Int -> Picture
drawBattleMenu assets battle options selectedIdx =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithBattleMenu options selectedIdx,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleConvertState :: World -> Battle -> ConvertState -> Picture
drawBattleConvertState World{assets} battle state
    | ConvertAnnounceAttempt <- state = drawFullBattleScreenWithAnnouncement assets battle ["You try to convert " ++ hName (attacker battle)]
    | ConvertAnnouncePokeball <- state = drawFullBattleScreenWithAnnouncement assets battle ["You throw vegan propaganda at " ++ hName (attacker battle)]
    | ConvertAnnounceSuccess newDefender <- state = drawFullBattleScreenWithAnnouncement assets battle [
            "You have converted " ++ hName (attacker battle),
            "  to " ++ hName newDefender
        ]
    | ConvertAnnounceFailure <- state = drawFullBattleScreenWithAnnouncement assets battle [hName (attacker battle) ++ " could not be converted"]

drawChooseDefenderMenu :: World -> [Human] -> Int -> Picture
drawChooseDefenderMenu World{battle = Just battle, assets} options selectedIdx =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithDefendersMenu options selectedIdx,
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawDefenderDefeated :: World -> Human -> Picture
drawDefenderDefeated World{battle = Just battle, assets} defender =
    let defending = getSelectedDefender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement [hName defender ++ " has been defeated.", "  Bringing in the next defender."],
            drawDefender assets defending,
            drawDefenderHP defending,
            drawAttacker assets attacking,
            drawAttackerHP attacking
        ]

drawBattleState :: World -> Picture
drawBattleState world@World{assets, battle = Just battle}
    | Challenge attacker <- bState battle = drawBattleChallenge assets attacker
    | DefenderAttackChoose <- bState battle = drawBattleAttackChoose assets battle
    | AnnounceAttack human attack <- bState battle = drawBattleAttackAnnounce assets battle human attack
    | AnnounceDamage attack <- bState battle = drawBattleDamageAnnounce assets battle attack
    | DefenderVictor attacker <- bState battle = drawBattleDefenderVictor assets battle attacker
    | AttackerVictor attacker <- bState battle = drawBattleAttackerVictor assets battle attacker
    | BattleMenuOpen options selectedIdx <- bState battle = drawBattleMenu assets battle options selectedIdx
    | Convert convertState <- bState battle = drawBattleConvertState world battle convertState
    | ChooseDefenderMenuOpen options selectedIdx <- bState battle = drawChooseDefenderMenu world options selectedIdx
    | AnnounceDefenderDefeated defender <- bState battle = drawDefenderDefeated world defender
drawBattleState World{battle = Nothing} = undefined

drawGameOver :: World -> Picture
drawGameOver World{assets} = asGameOver assets

draw :: World -> Picture
draw world@World{state = InField} = drawFieldState world
draw world@World{state = InBattle} = drawBattleState world
draw world@World{state = GameOver} = drawGameOver world

handleMoveInput :: Event -> World -> World
handleMoveInput (EventKey key GlossKey.Down _ _) world@World{cow, grid}
    | Char 'w' <- key = moveCow Up
    | SpecialKey KeyUp <- key = moveCow Up
    | Char 's' <- key = moveCow Down
    | SpecialKey KeyDown <- key = moveCow Down
    | Char 'a' <- key = moveCow Left
    | SpecialKey KeyLeft <- key = moveCow Left
    | Char 'd' <- key = moveCow Right
    | SpecialKey KeyRight <- key = moveCow Right
    where moveCow direction = world {cow = moveCowIfNotMoving grid cow direction}
handleMoveInput _ world = world

handleBattleChallengeInput :: Event -> World -> World
handleBattleChallengeInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle} = world {battle = Just $ battle {bState = DefenderAttackChoose}}
handleBattleChallengeInput _ world = world

moveSelectedAttack :: Int -> World -> World
moveSelectedAttack offset world@World{battle = Just battle} = world {battle = Just $ battle {selectedAttack = selectedAttack battle + offset}}

handleBattleSelectionInput :: Event -> World -> World
handleBattleSelectionInput (EventKey (Char 'w') GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle > 0 = moveSelectedAttack (-1) world
    | otherwise = world
handleBattleSelectionInput (EventKey (SpecialKey KeyUp) GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle > 0 = moveSelectedAttack (-1) world
    | otherwise = world
handleBattleSelectionInput (EventKey (Char 's') GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle < (length . attacks $ getSelectedDefender battle) - 1 = moveSelectedAttack 1 world
    | otherwise = world
handleBattleSelectionInput (EventKey (SpecialKey KeyDown) GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle < (length . attacks $ getSelectedDefender battle) - 1 = moveSelectedAttack 1 world
    | otherwise = world
handleBattleSelectionInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = (Just battle)} =
    world {battle = Just $ battle {bState = AnnounceAttack (getSelectedDefender battle) (getSelectedAttack battle)}}
handleBattleSelectionInput (EventKey (Char 'm') GlossKey.Up _ _) world@World{battle = Just battle} = world {
    battle = Just $ battle {bState = BattleMenuOpen [ContinueBattle, ConvertAttacker, ChooseDefender] 0}
}
handleBattleSelectionInput _ world = world

handleBattleAttackAnnounce :: Event -> World -> World
handleBattleAttackAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle@Battle{bState = AnnounceAttack _ attack}} =
    world {battle = Just $ battle {bState = AnnounceDamage attack}}
handleBattleAttackAnnounce _ world = world

handleBattleDamageAnnounce :: Event -> World -> World
handleBattleDamageAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{gen, battle = Just battle@Battle{bState = AnnounceDamage attack, turn = DefenderTurn}} =
    let battle' = applyAttackToBattle attack battle
        attacker' = attacker battle'
        won = hitPoints attacker' == 0
    in if won
        then world {battle = Just $ battle' {bState = DefenderVictor (attacker battle')}}
        else
            let (attackerAttack, gen') = getRandomAttack gen attacker'
            in world {
                    gen = gen',
                    battle = Just . swapTurn $ battle' {bState = AnnounceAttack attacker' attackerAttack}
                }
handleBattleDamageAnnounce (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle@Battle{bState = AnnounceDamage attack, turn = AttackerTurn}} =
    let battle' = applyAttackToBattle attack battle
        currentDefender = getSelectedDefender battle'
        won = hitPoints (getSelectedDefender battle') == 0
        remainingDefenders = removeSelectedDefender battle'
    in if | won && null remainingDefenders  -> world {battle = Just $ battle' {bState = AttackerVictor (attacker battle')}}
          | won                             -> world {battle = Just . swapTurn $ battle' {bState = AnnounceDefenderDefeated currentDefender, bSelectedDefender = 0, bDefenders = remainingDefenders}}
          | otherwise                       -> world {battle = Just . swapTurn $ battle' {bState = DefenderAttackChoose}}
handleBattleDamageAnnounce _ world = world

handleBattleDefenderVictor :: Event -> World -> World
handleBattleDefenderVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle} = backToField battle world
handleBattleDefenderVictor _ world = world

handleBattleAttackerVictor :: Event -> World -> World
handleBattleAttackerVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world =
    world {state = GameOver, battle = Nothing}
handleBattleAttackerVictor _ world = world

handleBattleMenuOpen :: Event -> World -> World
handleBattleMenuOpen event world@World{defenders, battle = Just battle@Battle{bState = BattleMenuOpen options selectedIdx}}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        case options !! selectedIdx of
            ContinueBattle -> world {battle = Just battle {bState = DefenderAttackChoose}}
            ConvertAttacker -> world {battle = Just battle {bState = Convert ConvertAnnounceAttempt}}
            ChooseDefender -> world {battle = Just battle {bState = ChooseDefenderMenuOpen defenders 0}}
    | EventKey (Char 'w') GlossKey.Up _ _ <- event,
      selectedIdx > 0 = world {battle = Just battle {bState = BattleMenuOpen options (selectedIdx - 1)}}
    | EventKey (SpecialKey KeyUp) GlossKey.Up _ _ <- event,
      selectedIdx > 0 = world {battle = Just battle {bState = BattleMenuOpen options (selectedIdx - 1)}}
    | EventKey (Char 's') GlossKey.Up _ _ <- event,
      selectedIdx < (length options - 1) = world {battle = Just battle {bState = BattleMenuOpen options (selectedIdx + 1)}}
    | EventKey (SpecialKey KeyDown) GlossKey.Up _ _ <- event,
      selectedIdx < (length options - 1) = world {battle = Just battle {bState = BattleMenuOpen options (selectedIdx + 1)}}
handleBattleMenuOpen _ world = world

handleBattleConvertStateInput :: ConvertState -> Event -> World -> World
handleBattleConvertStateInput ConvertAnnounceAttempt event world@World{battle = Just battle}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event = world {battle = Just battle {bState = Convert ConvertAnnouncePokeball}}
    | otherwise = world
handleBattleConvertStateInput ConvertAnnouncePokeball event world@World{gen, battle = Just battle@Battle{attacker}}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        let (captured, gen') = isCaptured gen attacker
            worldWithUpdatedGen = world {gen = gen'}
            (Just newDefender) = convertsTo attacker
        in if captured
            then worldWithUpdatedGen {battle = Just battle {bState = Convert (ConvertAnnounceSuccess newDefender)}}
            else worldWithUpdatedGen {battle = Just battle {bState = Convert ConvertAnnounceFailure}}
    | otherwise = world
handleBattleConvertStateInput (ConvertAnnounceSuccess newDefender) event world@World{battle = Just battle}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event = addDefender newDefender $ backToField battle world
    | otherwise = world
handleBattleConvertStateInput ConvertAnnounceFailure event world@World{gen, battle = Just battle@Battle{attacker}}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        let (attackerAttack, gen') = getRandomAttack gen attacker
        in world {
                gen = gen',
                battle = Just . swapTurn $ battle {bState = AnnounceAttack attacker attackerAttack}
            }
    | otherwise = world

handleDefenderMenuInput :: Event -> World -> [Human] -> Int -> World
handleDefenderMenuInput event world@World{gen, battle = Just battle@Battle{attacker}} options selectedIdx
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        let (attackerAttack, gen') = getRandomAttack gen attacker
        in world {
                gen = gen',
                battle = Just . swapTurn . swapDefender selectedIdx $ battle {bState = AnnounceAttack attacker attackerAttack}
            }
    | EventKey (Char 'w') GlossKey.Up _ _ <- event,
      selectedIdx > 0 = world {battle = Just battle {bState = ChooseDefenderMenuOpen options (selectedIdx - 1)}}
    | EventKey (SpecialKey KeyUp) GlossKey.Up _ _ <- event,
      selectedIdx > 0 = world {battle = Just battle {bState = ChooseDefenderMenuOpen options (selectedIdx - 1)}}
    | EventKey (Char 's') GlossKey.Up _ _ <- event,
      selectedIdx < (length options - 1) = world {battle = Just battle {bState = ChooseDefenderMenuOpen options (selectedIdx + 1)}}
    | EventKey (SpecialKey KeyDown) GlossKey.Up _ _ <- event,
      selectedIdx < (length options - 1) = world {battle = Just battle {bState = ChooseDefenderMenuOpen options (selectedIdx + 1)}}
    | otherwise = world

handleBattleDefenderDefeatedInput :: Event -> World -> World
handleBattleDefenderDefeatedInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle} = world {battle = Just battle{bState = DefenderAttackChoose}}
handleBattleDefenderDefeatedInput _ world = world

handleBattleInput :: Event -> World -> World
handleBattleInput event world@World{battle = Just battle}
    | Challenge _ <- bState battle = handleBattleChallengeInput event world
    | DefenderAttackChoose <- bState battle = handleBattleSelectionInput event world
    | AnnounceAttack _ _ <- bState battle = handleBattleAttackAnnounce event world
    | AnnounceDamage _ <- bState battle = handleBattleDamageAnnounce event world
    | DefenderVictor _ <- bState battle = handleBattleDefenderVictor event world
    | AttackerVictor _ <- bState battle = handleBattleAttackerVictor event world
    | BattleMenuOpen _ _ <- bState battle = handleBattleMenuOpen event world
    | Convert convertState <- bState battle = handleBattleConvertStateInput convertState event world
    | ChooseDefenderMenuOpen options selectedIdx <- bState battle = handleDefenderMenuInput event world options selectedIdx
    | AnnounceDefenderDefeated _ <- bState battle = handleBattleDefenderDefeatedInput event world
handleBattleInput _ world = world

handleInput :: Event -> World -> World
handleInput event world@World{state = InField} = handleMoveInput event world
handleInput event world@World{state = InBattle} = handleBattleInput event world
handleInput _ world@World{state = GameOver} = world

updateMovePosition :: World -> World
updateMovePosition world@World{gen, cow, stepsSinceBattle, defenders} =
    case movement cow of
        Start direction start end            -> world {cow = cow {movement = Moving direction 1 start end}}
        Moving _ 15 _ end                    ->
            let stepsSinceBattle' = stepsSinceBattle + 1
                (underAttack, gen') = isUnderAttack gen stepsSinceBattle'
                (randomAttacker, gen'') = getRandomAttacker gen'
                worldWithUpdatedCowAndGen = world {cow = cow {movement = Done end}, gen = gen''}
            in if underAttack
                then worldWithUpdatedCowAndGen {
                        stepsSinceBattle = 0,
                        state = InBattle,
                        battle = Just $ mkBattle defenders randomAttacker
                    }
                else worldWithUpdatedCowAndGen {
                        stepsSinceBattle = stepsSinceBattle'
                    }
        Moving direction iteration start end -> world {cow = cow {movement = Moving direction (iteration + 1) start end}}
        Done _                               -> world

updateCowImage :: Float -> World -> World
updateCowImage _ world@World{state, cow = cow@Cow{cowImageFlip, cowAssets}}
    | InField <- state =
        let cowImageFlip' = cowImageFlip + 1
            flipCowImage = cowImageFlip' `mod` 15 == 0
        in if flipCowImage
            then world{
                    cow = cow{
                        cowCurrentAsset = fst cowAssets,
                        cowAssets = swap cowAssets,
                        cowImageFlip = 0
                    }
                }
            else world{
                    cow = cow{cowImageFlip = cowImageFlip'}
                }
    | otherwise = world

updateFieldPosition :: Float -> World -> World
updateFieldPosition _ world@World{state = InField} = updateMovePosition world
updateFieldPosition _ world                        = world

windowHeight :: Grid -> Int
windowHeight Grid{..} = rows * displayRatio

windowWidth :: Grid -> Int
windowWidth Grid{..} = columns * displayRatio

playNmkp :: IO ()
playNmkp = do
    gen <- getStdGen
    let world@World{grid} = genesis gen
    play (InWindow "Nmkp / Graze Fighter / Plants vs Carnists" (windowWidth grid, windowHeight grid) (0, 0))
         white
         30
         world
         draw
         handleInput
         [
             updateFieldPosition,
             updateCowImage
         ]
