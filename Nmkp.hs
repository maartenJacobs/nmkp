{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternGuards #-}

import Graphics.Gloss.Game hiding (Up, Down)
import qualified Graphics.Gloss.Game as GlossKey
import Prelude hiding (Left, Right)
import System.Random

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
    hGetAsset :: (Assets -> Picture),
    attacks :: [Attack],
    convertsTo :: Maybe Human
}

data Assets = Assets {
    asCow :: Picture,
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

instance Show BattleMenuOption where
    show :: BattleMenuOption -> String
    show ContinueBattle = "Fight"
    show ConvertAttacker = "Convert"

data ConvertState = ConvertAnnounceAttempt
                  | ConvertAnnouncePokeball
                  | ConvertAnnounceSuccess Human -- New defender
                  | ConvertAnnounceFailure

data BattleState = Challenge Human -- "Attacker name" wants to eat you
                 | DefenderAttackChoose
                 | AnnounceAttack Human Attack -- "Human name" uses "attack name"
                 | AnnounceDamage Attack -- "attack name" inflicted "x" damage
                 | DefenderVictor Human -- "Attacker name" has been defeated
                 | AttackerVictor Human -- You have been defeated by "attacker name"!
                 | BattleMenuOpen [BattleMenuOption] Int
                 | Convert ConvertState

data Battle = Battle {
    turn :: Turn,
    defender :: Human,
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
mkDefender name hitPoints getAsset attacks = mkHuman name hitPoints getAsset Nothing attacks

mkAttacker :: String -> Int -> (Assets -> Picture) -> Human -> [Attack] -> Human
mkAttacker name hitPoints getAsset convertsTo attacks = mkHuman name hitPoints getAsset (Just convertsTo) attacks

mkBattle :: Human -> Human -> Battle
mkBattle defender attacker = Battle {
    turn = DefenderTurn,
    defender = defender,
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

obeseMan :: Human
obeseMan =
    let fatVegan = mkDefender "Fat vegan" 100 asFatVegan [
                Attack {
                    attName = "Vegan meals are so cheap",
                    damage = 10
                },
                Attack {
                    attName = "Facon 4 lyfe",
                    damage = 10
                }
            ]
    in mkAttacker "Obese man" 100 asObeseMan fatVegan [
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

dairyFarmer :: Human
dairyFarmer =
    let nutMilker = mkDefender "Nut milker" 55 asNutMilker [
                Attack {
                    attName = "Grind nuts",
                    damage = 5
                },
                Attack {
                    attName = "Spray mylk",
                    damage = 8
                }
            ]
    in mkAttacker "Dairy farmer" 50 asDairyFarmer nutMilker [
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

moveCowIfNotMoving :: Grid -> Direction -> Cow -> Cow
moveCowIfNotMoving grid direction cow
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
        asGrass = scale 2 2 $ png "./assets/grass.png",
        asVegan = scale 5.075 5.075 $ png "./assets/vegan.png",
        asCarnist = scale 4.7 4.7 $ png "./assets/carnist.png",
        asGameOver = png "./assets/gameover.png",
        asDairyFarmer = scale 4.7 4.7 $ png "./assets/dairy-farmer.png",
        asObeseMan = scale 4.7 4.7 $ png "./assets/obese-man.png",
        asFatVegan = scale 4.7 4.7 $ png "./assets/fat-vegan.png",
        asNutMilker = scale 4.7 4.7 $ png "./assets/nut-milker.png"
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

backToField :: World -> World
backToField world = world {state = InField, battle = Nothing}

addDefender :: Human -> World -> World
addDefender defender world@World{defenders} = world {defenders = defender : defenders}

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
    in pictures $ (color black $ line [(-336, -134), (336, -134)]) : translatedMsgs

drawMenuScreenWithBattleMenu :: [BattleMenuOption] -> Int -> Picture
drawMenuScreenWithBattleMenu options selectedIdx =
    let labels = map show options
        prepareLabel y = color black . translate 152 y . scale 0.2 0.2 . text
        labelsWithIndex = zip [0..] labels
        translatedLabels = map (\(idx, label) -> prepareLabel (-180 - idx * 40) label) labelsWithIndex
        selectedOffset = fromIntegral (-170 - selectedIdx * 40)
    in pictures $
        translatedLabels ++ [
            color black $ line [(-336, -134), (336, -134)],
            color black $ line [(112, -134), (112, -336)],
            color black . translate 130 selectedOffset $ thickCircle 0 20
        ]

drawFullBattleScreenWithAnnouncement :: Assets -> Battle -> [String] -> Picture
drawFullBattleScreenWithAnnouncement assets battle msgs =
    let defending = defender battle
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
        drawMenuScreenWithAnnouncement [(hName attacker ++ " wants to eat you")],
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
drawBattleAttackAnnounce assets battle Human{hName} Attack{attName} = drawFullBattleScreenWithAnnouncement assets battle [hName ++ " used", "  " ++ attName]

drawBattleDamageAnnounce :: Assets -> Battle -> Attack -> Picture
drawBattleDamageAnnounce assets battle Attack{attName, damage} = drawFullBattleScreenWithAnnouncement assets battle [attName, "  inflicted " ++ show damage]

drawBattleDefenderVictor :: Assets -> Battle -> Human -> Picture
drawBattleDefenderVictor assets battle Human{hName} =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement [hName ++ " has been defeated"],
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleAttackerVictor :: Assets -> Battle -> Human -> Picture
drawBattleAttackerVictor assets battle Human{hName} =
    let defending = defender battle
        attacking = attacker battle
    in pictures [
            drawMenuScreenWithAnnouncement ["You have been defeated by ", hName ++ "!"],
            drawDefender assets defending,
            drawAttacker assets attacking
        ]

drawBattleMenu :: Assets -> Battle -> [BattleMenuOption] -> Int -> Picture
drawBattleMenu assets battle options selectedIdx =
    let defending = defender battle
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
    | ConvertAnnounceAttempt <- state = drawFullBattleScreenWithAnnouncement assets battle ["You try to convert " ++ (hName $ attacker battle)]
    | ConvertAnnouncePokeball <- state = drawFullBattleScreenWithAnnouncement assets battle ["You throw vegan propaganda at " ++ (hName $ attacker battle)]
    | ConvertAnnounceSuccess newDefender <- state = drawFullBattleScreenWithAnnouncement assets battle [
            "You have converted " ++ (hName $ attacker battle),
            "  to " ++ hName newDefender
        ]
    | ConvertAnnounceFailure <- state = drawFullBattleScreenWithAnnouncement assets battle [(hName $ attacker battle) ++ " could not be converted"]

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
drawBattleState World{battle = Nothing} = undefined

drawGameOver :: World -> Picture
drawGameOver World{assets} = asGameOver assets

draw :: World -> Picture
draw world@World{state = InField} = drawFieldState world
draw world@World{state = InBattle} = drawBattleState world
draw world@World{state = GameOver} = drawGameOver world

handleMoveInput :: Event -> World -> World
handleMoveInput (EventKey (Char 'w') GlossKey.Down _ _) world@World{cow, grid} = world {cow = moveCowIfNotMoving grid Up cow}
handleMoveInput (EventKey (Char 's') GlossKey.Down _ _) world@World{cow, grid} = world {cow = moveCowIfNotMoving grid Down cow}
handleMoveInput (EventKey (Char 'a') GlossKey.Down _ _) world@World{cow, grid} = world {cow = moveCowIfNotMoving grid Left cow}
handleMoveInput (EventKey (Char 'd') GlossKey.Down _ _) world@World{cow, grid} = world {cow = moveCowIfNotMoving grid Right cow}
handleMoveInput _ world = world

handleBattleChallengeInput :: Event -> World -> World
handleBattleChallengeInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = Just battle} = world {battle = Just $ battle {bState = DefenderAttackChoose}}
handleBattleChallengeInput _ world = world

handleBattleSelectionInput :: Event -> World -> World
handleBattleSelectionInput (EventKey (Char 'w') GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle > 0 = world {battle = Just $ battle {selectedAttack = selectedAttack battle - 1}}
    | otherwise = world
handleBattleSelectionInput (EventKey (Char 's') GlossKey.Up _ _) world@World{battle = (Just battle)}
    | selectedAttack battle < (length . attacks $ defender battle) - 1 = world {battle = Just $ battle {selectedAttack = selectedAttack battle + 1}}
    | otherwise = world
handleBattleSelectionInput (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world@World{battle = (Just battle)} =
    world {battle = Just $ battle {bState = AnnounceAttack (defender battle) (getSelectedAttack battle)}}
handleBattleSelectionInput (EventKey (Char 'm') GlossKey.Up _ _) world@World{battle = Just battle} = world {
    battle = Just $ battle {bState = BattleMenuOpen [ContinueBattle, ConvertAttacker] 0}
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
        won = hitPoints (defender battle') == 0
    in if won
        then world {battle = Just $ battle' {bState = AttackerVictor (attacker battle')}}
        else world {battle = Just . swapTurn $ battle' {bState = DefenderAttackChoose}}
handleBattleDamageAnnounce _ world = world

handleBattleDefenderVictor :: Event -> World -> World
handleBattleDefenderVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world = backToField world
handleBattleDefenderVictor _ world = world

handleBattleAttackerVictor :: Event -> World -> World
handleBattleAttackerVictor (EventKey (SpecialKey KeyEnter) GlossKey.Up _ _) world =
    world {state = GameOver, battle = Nothing}
handleBattleAttackerVictor _ world = world

handleBattleMenuOpen :: Event -> World -> World
handleBattleMenuOpen event world@World{battle = Just battle@Battle{bState = BattleMenuOpen options selectedIdx}}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        case options !! selectedIdx of
            ContinueBattle -> world {battle = Just battle {bState = DefenderAttackChoose}}
            ConvertAttacker -> world {battle = Just battle {bState = Convert ConvertAnnounceAttempt}}
    | EventKey (Char 'w') GlossKey.Up _ _ <- event,
      selectedIdx > 0 = world {battle = Just battle {bState = BattleMenuOpen options (selectedIdx - 1)}}
    | EventKey (Char 's') GlossKey.Up _ _ <- event,
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
handleBattleConvertStateInput (ConvertAnnounceSuccess newDefender) event world
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event = addDefender newDefender $ backToField world
    | otherwise = world
handleBattleConvertStateInput ConvertAnnounceFailure event world@World{gen, battle = Just battle@Battle{attacker}}
    | EventKey (SpecialKey KeyEnter) GlossKey.Up _ _ <- event =
        let (attackerAttack, gen') = getRandomAttack gen attacker
        in world {
                gen = gen',
                battle = Just . swapTurn $ battle {bState = AnnounceAttack attacker attackerAttack}
            }
    | otherwise = world

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
                        battle = Just $ mkBattle (head defenders) randomAttacker
                    }
                else worldWithUpdatedCowAndGen {
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
    gen <- getStdGen
    let world@World{grid} = genesis gen
    play (InWindow "Nmkp / Graze Fighter / Plants vs Carnists" (windowWidth grid, windowHeight grid) (0, 0))
         white
         30
         world
         draw
         handleInput
         [updateFieldPosition]
