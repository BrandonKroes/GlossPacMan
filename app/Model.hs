{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
module Model where



import Paths_pacman
import Graphics.Gloss.Juicy



import Constants
import Data.List
import Data.Maybe
import Data.Aeson (ToJSON)
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Gloss


import GHC.Generics


type Assets = Map String Picture

data GameState = GameState
  { runningState     :: RunningState,
    player           :: Player,
    world            :: World,
    ghosts           :: Ghosts,
    consumablesLeft  :: Int,
    consumablesTotal :: Int,
    time             :: Float,
    animationTime    :: Float,
    animationInterval:: Int,
    assets           :: Assets
  }

-- * The default state for a running game.
-- * the assets are intentionally left blank, because these will be inserted upon switching to the running state
runningGameState :: GameState
runningGameState = GameState
    { runningState = RUNNING,
      player = PacMan (14, 23) 0 (UP, NOTHING) ,
      ghosts =
        [ Ghost (14, 12) RED    Idle 0.0 0 UP,
          Ghost (14, 14) ORANGE Idle 0.0 0 UP,
          Ghost (15, 14) PINK   Idle 0.0 0 UP,
          Ghost (16, 14) CYAN   Idle 0.0 0 UP
        ],
      world = getDefaultWorld,
      consumablesTotal = countAmountOfDots getDefaultWorld,
      consumablesLeft = countAmountOfDots getDefaultWorld,
      time=0.0,
      animationTime = 0,
      animationInterval=1,
      assets = undefined
    }

-- * copying the available assets
switchToRunningState::GameState -> GameState
switchToRunningState baseState = adjustedState
  where
    newState = runningGameState
    adjustedState = newState {assets = (assets baseState)}

-- * Default start of the game
initialGameState :: Assets -> GameState
initialGameState textures = GameState
      { runningState = START,
        player = PacMan (14, 23) 0 (UP, NOTHING),
        ghosts =
          [
          ],
        world = [],
        consumablesTotal = 0,
        consumablesLeft =  0,
        time=0.0,
        animationTime = 0,
        animationInterval=1,
        assets = textures
      }




type GhostBehaviour = (Int, GhostState)

data RunningState = START | RUNNING | WON | LOST | PAUSE deriving (Show, Eq)

-- * Checking if the game is running
isPlayState::GameState->Bool
isPlayState gstate | (isPaused gstate) = True
                   | RUNNING == runningState gstate = True
                   | otherwise= False

-- * Checking if the game is in paused mode
isPaused::GameState->Bool
isPaused gstate =  PAUSE == runningState gstate

-- * used to switch between running and pausing
flipPause::GameState->GameState
flipPause gstate | isPaused gstate = gstate {runningState=RUNNING}
                 | otherwise = gstate{runningState=PAUSE}


data Player
  = PacMan
      { position :: (Int, Int),
        score :: Int,
        direction :: (Direction, Direction)
      }
  | Ghost
      { position  :: (Int, Int),
        gColor    :: GhostColor,
        state     :: GhostState,
        timestamp :: Float,
        sequence  :: Int,
        gDirection :: Direction
      }

type Ghosts = [Player]

data GhostColor = RED | ORANGE | PINK | CYAN deriving (Show, Eq)

-- * Get the position of all the ghosts
getGhostsPosition::Ghosts->[(Int, Int)]
getGhostsPosition ghosts = map getPlayerPosition ghosts

-- * Get the position of a player by player
getPlayerPosition::Player->(Int, Int)
getPlayerPosition player = position player

-- Tile interactions
data Tile = Walkable Field (Int, Int) | NotWalkable WallType (Int, Int)

-- * Check if a tile is walkable
isWalkable :: Tile -> Bool
isWalkable (Walkable _ _) = True
isWalkable _ = False


-- Only the Walkable tiles have the field so no pm is required.
setTileField::Tile->Field-> Tile
setTileField (Walkable field position) nField = Walkable nField position
setTileField t _ = t


-- * Check if a field is a coin or dot
isConsumable::Tile->Bool
isConsumable (Walkable field _) = field /= Empty && field /= DOOR
isConsumable _ = False


-- * Check if a field is a dot
isDot::Tile->Bool
isDot (Walkable field _) = field == Dot || field == Coin
isDot _ = False

-- * Check if a field is a coin
isCoin::Tile->Bool
isCoin (Walkable field _) = field == Coin
isCoin _ = False

data Field = Coin | Empty | Dot | DOOR deriving (Show, Eq)

type World = [Tile]

-- * Count the amount of dots that are in the world
countAmountOfDots::World->Int
countAmountOfDots w = length $ filter (isDot) w

-- * Acquiring a specific tile from the tiles list via position
getTileByPosition::GameState -> (Int, Int)->Tile
getTileByPosition gstate index = fromJust . find(isTile index) $ world gstate

-- * Acquiring a specific tile index from the tiles list via position
getTileIndexByPosition::GameState -> (Int, Int)->Int
getTileIndexByPosition gstate index = fromJust . findIndex(isTile index) $ world gstate

-- * Based on the tiles nearby, checking if they are walkable.
getWalkableNeighborTilePositions::GameState->(Int, Int)->[(Int, Int)]
getWalkableNeighborTilePositions gstate position = map getPositionFromTile $ getWalkableNeighborTile gstate position

-- * individually checking if the tile is a walkable
getWalkableNeighborTile::GameState->(Int,Int)->[Tile]
getWalkableNeighborTile gstate position = filter (isWalkable) $ getNeighborTile gstate position

-- * Get all the tiles that a player COULD go to based on the 4 directions that are possible.
getNeighborTile::GameState->(Int, Int) -> [Tile]
getNeighborTile gstate (x, y) =
  getSafeTile gstate (x-1, y) ++
  getSafeTile gstate (x+1, y) ++
  getSafeTile gstate (x, y-1) ++
  getSafeTile gstate (x, y+1)

-- * Preventing player instances from leaving the map
getSafeTile::GameState->(Int, Int) -> [Tile]
getSafeTile gstate (_, 29) = []
getSafeTile gstate (27, _) = []
getSafeTile gstate (_, 2) = []
getSafeTile gstate (2, _) = []
getSafeTile gstate pos = [getTileByPosition gstate pos]


getPositionFromTile::Tile->(Int, Int)
getPositionFromTile (Walkable field position) = position
getPositionFromTile (NotWalkable _ position) = position

isTile::(Int, Int) -> Tile -> Bool
isTile checkPos (Walkable field position) = checkPos == position
isTile checkPos (NotWalkable _ position) = checkPos == position


data Direction =  UP   | DOWN  | LEFT       | RIGHT      | NOTHING deriving (Show, Eq)
data GhostState = Idle | Chase | Retreat    | Frightened | Scatter [Direction] | ToScatterPlace deriving (Show, Eq)


-- Bulk updating ghosts to a state and checking if you're allowed to do it.
setGhostsToState::GhostState->[Player]->Float->[Player]
setGhostsToState ghostState ghosts time = nGhosts
  where
    nonApplicableGhosts = filter (\g -> not (ghostTransitionAllowed ghostState (state g))) ghosts
    adjustableGhosts = filter (\g -> ghostTransitionAllowed ghostState (state g)) ghosts
    applicableGhosts  =   map (setGhostToState ghostState time) adjustableGhosts
    nGhosts = applicableGhosts ++ nonApplicableGhosts

-- Implicitly forcing a ghost into a new state accomponied with the timestamp for activity trigger
setGhostToState::GhostState->Float->Player->Player
setGhostToState nState time = \g -> g {state=nState, timestamp=time}

-- * Checking if switching the state of a ghost is allowed
ghostTransitionAllowed::GhostState->GhostState->Bool
ghostTransitionAllowed Frightened Idle = False
ghostTransitionAllowed _  _ = True


-- * Checking if a ghost is non lethal
isNonLethal::Player->Bool
isNonLethal (Ghost _ _ state _ _ _) = elem state getNonLethalGhostStates


-- * Checking if a ghost is lethal
isLethal::Player->Bool
isLethal ghost = not $ isNonLethal ghost


-- * Checking if a ghost is a specific state
isStateGhost::GhostState->Player->Bool
isStateGhost state (Ghost _ _ gState _ _ _) = gState == state



-- * Checking if a ghost is a specific state
isNotStateGhost::GhostState->Player->Bool
isNotStateGhost state g@(Ghost _ _ gState _ _ _) = gState /= state



-- * Checking if a player type is not on another player type
samePosition::(Int, Int) -> Player -> Bool
samePosition pos = \x -> (position x) == pos


-- * Checking if a player type isn't on another player type
notSamePosition::(Int, Int) -> Player -> Bool
notSamePosition pos = \x -> (position x) /= pos


-- * Checking if a frightened ghost is located on top of a player
frightenedGhostsOnPlayer::[Player] -> [Player]
frightenedGhostsOnPlayer players = filter (isStateGhost Frightened) players


-- * The states that define if a player can't be hurt.
getNonLethalGhostStates::[GhostState]
getNonLethalGhostStates = [Retreat, Frightened]

-- * The positions of the ghosts that can't hurt the player
getNonLethalGhosts::[Player] -> [Player]
getNonLethalGhosts ghosts = filter (isNonLethal) ghosts


-- * The positions of the ghosts that can hurt the players
getDeadlyGhostsPosition::Ghosts -> [(Int, Int)]
getDeadlyGhostsPosition players = getGhostsPosition $ filter (isLethal) players

-- the total route the ghost need to walk (first one is always double)
getTotalRoute :: Player -> [Direction]
getTotalRoute (Ghost _ ORANGE _ _ _ _) = [RIGHT, RIGHT, UP, LEFT, UP, LEFT, DOWN, LEFT, DOWN]
getTotalRoute (Ghost _ CYAN _ _ _ _) = [LEFT, LEFT, UP, RIGHT, UP, RIGHT, DOWN, RIGHT, DOWN]
getTotalRoute (Ghost _ PINK _ _ _ _) = [DOWN, DOWN, RIGHT, UP, LEFT]
getTotalRoute (Ghost _ RED _ _ _ _) = [DOWN, DOWN, LEFT, UP, RIGHT]

-- ** The start positions for the ghosts
getScatterStart :: GhostColor -> (Int, Int)
getScatterStart RED = (27,2)
getScatterStart PINK = (2,2)
getScatterStart ORANGE = (2,29)
getScatterStart CYAN = (27,29)


data WallType = VERTICAL | LANGLE | RANGLE | HORIZONTAL deriving (Show, Eq)




-- The default world (Keep in mind that this goes on for a while, its 868 records of map)
getDefaultWorld :: World
getDefaultWorld =
  [ NotWalkable HORIZONTAL (1, 1),
    NotWalkable HORIZONTAL (2, 1),
    NotWalkable HORIZONTAL (3, 1),
    NotWalkable HORIZONTAL (4, 1),
    NotWalkable HORIZONTAL (5, 1),
    NotWalkable HORIZONTAL (6, 1),
    NotWalkable HORIZONTAL (7, 1),
    NotWalkable HORIZONTAL (8, 1),
    NotWalkable HORIZONTAL (9, 1),
    NotWalkable HORIZONTAL (10, 1),
    NotWalkable HORIZONTAL (11, 1),
    NotWalkable HORIZONTAL (12, 1),
    NotWalkable HORIZONTAL (13, 1),
    NotWalkable HORIZONTAL (14, 1),
    NotWalkable HORIZONTAL (15, 1),
    NotWalkable HORIZONTAL (16, 1),
    NotWalkable HORIZONTAL (17, 1),
    NotWalkable HORIZONTAL (18, 1),
    NotWalkable HORIZONTAL (19, 1),
    NotWalkable HORIZONTAL (20, 1),
    NotWalkable HORIZONTAL (21, 1),
    NotWalkable HORIZONTAL (22, 1),
    NotWalkable HORIZONTAL (23, 1),
    NotWalkable HORIZONTAL (24, 1),
    NotWalkable HORIZONTAL (25, 1),
    NotWalkable HORIZONTAL (26, 1),
    NotWalkable HORIZONTAL (27, 1),
    NotWalkable HORIZONTAL (28, 1),
    NotWalkable VERTICAL (1, 2),
    Walkable Dot (2, 2),
    Walkable Dot (3, 2),
    Walkable Dot (4, 2),
    Walkable Dot (5, 2),
    Walkable Dot (6, 2),
    Walkable Dot (7, 2),
    Walkable Dot (8, 2),
    Walkable Dot (9, 2),
    Walkable Dot (10, 2),
    Walkable Dot (11, 2),
    Walkable Dot (12, 2),
    Walkable Dot (13, 2),
    NotWalkable VERTICAL (14, 2),
    NotWalkable VERTICAL (15, 2),
    Walkable Dot (16, 2),
    Walkable Dot (17, 2),
    Walkable Dot (18, 2),
    Walkable Dot (19, 2),
    Walkable Dot (20, 2),
    Walkable Dot (21, 2),
    Walkable Dot (22, 2),
    Walkable Dot (23, 2),
    Walkable Dot (24, 2),
    Walkable Dot (25, 2),
    Walkable Dot (26, 2),
    Walkable Dot (27, 2),
    NotWalkable VERTICAL (28, 2),
    NotWalkable VERTICAL (1, 3),
    Walkable Dot (2, 3),
    NotWalkable HORIZONTAL (3, 3),
    NotWalkable HORIZONTAL (4, 3),
    NotWalkable HORIZONTAL (5, 3),
    NotWalkable HORIZONTAL (6, 3),
    Walkable Dot (7, 3),
    NotWalkable HORIZONTAL (8, 3),
    NotWalkable HORIZONTAL (9, 3),
    NotWalkable HORIZONTAL (10, 3),
    NotWalkable HORIZONTAL (11, 3),
    NotWalkable HORIZONTAL (12, 3),
    Walkable Dot (13, 3),
    NotWalkable VERTICAL (14, 3),
    NotWalkable VERTICAL (15, 3),
    Walkable Dot (16, 3),
    NotWalkable HORIZONTAL (17, 3),
    NotWalkable HORIZONTAL (18, 3),
    NotWalkable HORIZONTAL (19, 3),
    NotWalkable HORIZONTAL (20, 3),
    NotWalkable HORIZONTAL (21, 3),
    Walkable Dot (22, 3),
    NotWalkable HORIZONTAL (23, 3),
    NotWalkable HORIZONTAL (24, 3),
    NotWalkable HORIZONTAL (25, 3),
    NotWalkable HORIZONTAL (26, 3),
    Walkable Dot (27, 3),
    NotWalkable VERTICAL (28, 3),
    NotWalkable VERTICAL (1, 4),
    Walkable Coin (2, 4),
    NotWalkable HORIZONTAL (3, 4),
    NotWalkable HORIZONTAL (4, 4),
    NotWalkable HORIZONTAL (5, 4),
    NotWalkable HORIZONTAL (6, 4),
    Walkable Dot (7, 4),
    NotWalkable HORIZONTAL (8, 4),
    NotWalkable HORIZONTAL (9, 4),
    NotWalkable HORIZONTAL (10, 4),
    NotWalkable HORIZONTAL (11, 4),
    NotWalkable HORIZONTAL (12, 4),
    Walkable Dot (13, 4),
    NotWalkable VERTICAL (14, 4),
    NotWalkable VERTICAL (15, 4),
    Walkable Dot (16, 4),
    NotWalkable HORIZONTAL (17, 4),
    NotWalkable HORIZONTAL (18, 4),
    NotWalkable HORIZONTAL (19, 4),
    NotWalkable HORIZONTAL (20, 4),
    NotWalkable HORIZONTAL (21, 4),
    Walkable Dot (22, 4),
    NotWalkable HORIZONTAL (23, 4),
    NotWalkable HORIZONTAL (24, 4),
    NotWalkable HORIZONTAL (25, 4),
    NotWalkable HORIZONTAL (26, 4),
    Walkable Coin (27, 4),
    NotWalkable VERTICAL (28, 4),
    NotWalkable VERTICAL (1, 5),
    Walkable Dot (2, 5),
    NotWalkable HORIZONTAL (3, 5),
    NotWalkable HORIZONTAL (4, 5),
    NotWalkable HORIZONTAL (5, 5),
    NotWalkable HORIZONTAL (6, 5),
    Walkable Dot (7, 5),
    NotWalkable HORIZONTAL (8, 5),
    NotWalkable HORIZONTAL (9, 5),
    NotWalkable HORIZONTAL (10, 5),
    NotWalkable HORIZONTAL (11, 5),
    NotWalkable HORIZONTAL (12, 5),
    Walkable Dot (13, 5),
    NotWalkable VERTICAL (14, 5),
    NotWalkable VERTICAL (15, 5),
    Walkable Dot (16, 5),
    NotWalkable HORIZONTAL (17, 5),
    NotWalkable HORIZONTAL (18, 5),
    NotWalkable HORIZONTAL (19, 5),
    NotWalkable HORIZONTAL (20, 5),
    NotWalkable HORIZONTAL (21, 5),
    Walkable Dot (22, 5),
    NotWalkable HORIZONTAL (23, 5),
    NotWalkable HORIZONTAL (24, 5),
    NotWalkable HORIZONTAL (25, 5),
    NotWalkable HORIZONTAL (26, 5),
    Walkable Dot (27, 5),
    NotWalkable VERTICAL (28, 5),
    NotWalkable VERTICAL (1, 6),
    Walkable Dot (2, 6),
    Walkable Dot (3, 6),
    Walkable Dot (4, 6),
    Walkable Dot (5, 6),
    Walkable Dot (6, 6),
    Walkable Dot (7, 6),
    Walkable Dot (8, 6),
    Walkable Dot (9, 6),
    Walkable Dot (10, 6),
    Walkable Dot (11, 6),
    Walkable Dot (12, 6),
    Walkable Dot (13, 6),
    Walkable Dot (14, 6),
    Walkable Dot (15, 6),
    Walkable Dot (16, 6),
    Walkable Dot (17, 6),
    Walkable Dot (18, 6),
    Walkable Dot (19, 6),
    Walkable Dot (20, 6),
    Walkable Dot (21, 6),
    Walkable Dot (22, 6),
    Walkable Dot (23, 6),
    Walkable Dot (24, 6),
    Walkable Dot (25, 6),
    Walkable Dot (26, 6),
    Walkable Dot (27, 6),
    NotWalkable VERTICAL (28, 6),
    NotWalkable VERTICAL (1, 7),
    Walkable Dot (2, 7),
    NotWalkable HORIZONTAL (3, 7),
    NotWalkable HORIZONTAL (4, 7),
    NotWalkable HORIZONTAL (5, 7),
    NotWalkable HORIZONTAL (6, 7),
    Walkable Dot (7, 7),
    NotWalkable HORIZONTAL (8, 7),
    NotWalkable HORIZONTAL (9, 7),
    Walkable Dot (10, 7),
    NotWalkable HORIZONTAL (11, 7),
    NotWalkable HORIZONTAL (12, 7),
    NotWalkable HORIZONTAL (13, 7),
    NotWalkable HORIZONTAL (14, 7),
    NotWalkable HORIZONTAL (15, 7),
    NotWalkable HORIZONTAL (16, 7),
    NotWalkable HORIZONTAL (17, 7),
    NotWalkable HORIZONTAL (18, 7),
    Walkable Dot (19, 7),
    NotWalkable HORIZONTAL (20, 7),
    NotWalkable HORIZONTAL (21, 7),
    Walkable Dot (22, 7),
    NotWalkable HORIZONTAL (23, 7),
    NotWalkable HORIZONTAL (24, 7),
    NotWalkable HORIZONTAL (25, 7),
    NotWalkable HORIZONTAL (26, 7),
    Walkable Dot (27, 7),
    NotWalkable VERTICAL (28, 7),
    NotWalkable VERTICAL (1, 8),
    Walkable Dot (2, 8),
    NotWalkable HORIZONTAL (3, 8),
    NotWalkable HORIZONTAL (4, 8),
    NotWalkable HORIZONTAL (5, 8),
    NotWalkable HORIZONTAL (6, 8),
    Walkable Dot (7, 8),
    NotWalkable HORIZONTAL (8, 8),
    NotWalkable HORIZONTAL (9, 8),
    Walkable Dot (10, 8),
    NotWalkable HORIZONTAL (11, 8),
    NotWalkable HORIZONTAL (12, 8),
    NotWalkable HORIZONTAL (13, 8),
    NotWalkable HORIZONTAL (14, 8),
    NotWalkable HORIZONTAL (15, 8),
    NotWalkable HORIZONTAL (16, 8),
    NotWalkable HORIZONTAL (17, 8),
    NotWalkable HORIZONTAL (18, 8),
    Walkable Dot (19, 8),
    NotWalkable HORIZONTAL (20, 8),
    NotWalkable HORIZONTAL (21, 8),
    Walkable Dot (22, 8),
    NotWalkable HORIZONTAL (23, 8),
    NotWalkable HORIZONTAL (24, 8),
    NotWalkable HORIZONTAL (25, 8),
    NotWalkable HORIZONTAL (26, 8),
    Walkable Dot (27, 8),
    NotWalkable VERTICAL (28, 8),
    NotWalkable VERTICAL (1, 9),
    Walkable Dot (2, 9),
    Walkable Dot (3, 9),
    Walkable Dot (4, 9),
    Walkable Dot (5, 9),
    Walkable Dot (6, 9),
    Walkable Dot (7, 9),
    NotWalkable HORIZONTAL (8, 9),
    NotWalkable HORIZONTAL (9, 9),
    Walkable Dot (10, 9),
    Walkable Dot (11, 9),
    Walkable Dot (12, 9),
    Walkable Dot (13, 9),
    NotWalkable VERTICAL (14, 9),
    NotWalkable VERTICAL (15, 9),
    Walkable Dot (16, 9),
    Walkable Dot (17, 9),
    Walkable Dot (18, 9),
    Walkable Dot (19, 9),
    NotWalkable HORIZONTAL (20, 9),
    NotWalkable HORIZONTAL (21, 9),
    Walkable Dot (22, 9),
    Walkable Dot (23, 9),
    Walkable Dot (24, 9),
    Walkable Dot (25, 9),
    Walkable Dot (26, 9),
    Walkable Dot (27, 9),
    NotWalkable VERTICAL (28, 9),
    NotWalkable HORIZONTAL (1, 10),
    NotWalkable HORIZONTAL (2, 10),
    NotWalkable HORIZONTAL (3, 10),
    NotWalkable HORIZONTAL (4, 10),
    NotWalkable HORIZONTAL (5, 10),
    NotWalkable HORIZONTAL (6, 10),
    Walkable Dot (7, 10),
    NotWalkable HORIZONTAL (8, 10),
    NotWalkable HORIZONTAL (9, 10),
    NotWalkable HORIZONTAL (10, 10),
    NotWalkable HORIZONTAL (11, 10),
    NotWalkable HORIZONTAL (12, 10),
    Walkable Empty (13, 10),
    NotWalkable HORIZONTAL (14, 10),
    NotWalkable HORIZONTAL (15, 10),
    Walkable Empty (16, 10),
    NotWalkable HORIZONTAL (17, 10),
    NotWalkable HORIZONTAL (18, 10),
    NotWalkable HORIZONTAL (19, 10),
    NotWalkable HORIZONTAL (20, 10),
    NotWalkable HORIZONTAL (21, 10),
    Walkable Dot (22, 10),
    NotWalkable HORIZONTAL (23, 10),
    NotWalkable HORIZONTAL (24, 10),
    NotWalkable HORIZONTAL (25, 10),
    NotWalkable HORIZONTAL (26, 10),
    NotWalkable HORIZONTAL (27, 10),
    NotWalkable HORIZONTAL (28, 10),
    Walkable Empty (1, 11),
    Walkable Empty (2, 11),
    Walkable Empty (3, 11),
    Walkable Empty (4, 11),
    Walkable Empty (5, 11),
    NotWalkable VERTICAL (6, 11),
    Walkable Dot (7, 11),
    NotWalkable HORIZONTAL (8, 11),
    NotWalkable HORIZONTAL (9, 11),
    NotWalkable HORIZONTAL (10, 11),
    NotWalkable HORIZONTAL (11, 11),
    NotWalkable HORIZONTAL (12, 11),
    Walkable Empty (13, 11),
    NotWalkable HORIZONTAL (14, 11),
    NotWalkable HORIZONTAL (15, 11),
    Walkable Empty (16, 11),
    NotWalkable HORIZONTAL (17, 11),
    NotWalkable HORIZONTAL (18, 11),
    NotWalkable HORIZONTAL (19, 11),
    NotWalkable HORIZONTAL (20, 11),
    NotWalkable HORIZONTAL (21, 11),
    Walkable Dot (22, 11),
    NotWalkable VERTICAL (23, 11),
    Walkable Empty (24, 11),
    Walkable Empty (25, 11),
    Walkable Empty (26, 11),
    Walkable Empty (27, 11),
    Walkable Empty (28, 11),
    Walkable Empty (1, 12),
    Walkable Empty (2, 12),
    Walkable Empty (3, 12),
    Walkable Empty (4, 12),
    Walkable Empty (5, 12),
    NotWalkable VERTICAL (6, 12),
    Walkable Dot (7, 12),
    NotWalkable HORIZONTAL (8, 12),
    NotWalkable HORIZONTAL (9, 12),
    Walkable Empty (10, 12),
    Walkable Empty (11, 12),
    Walkable Empty (12, 12),
    Walkable Empty (13, 12),
    Walkable Empty (14, 12),
    Walkable Empty (15, 12),
    Walkable Empty (16, 12),
    Walkable Empty (17, 12),
    Walkable Empty (18, 12),
    Walkable Empty (19, 12),
    NotWalkable HORIZONTAL (20, 12),
    NotWalkable HORIZONTAL (21, 12),
    Walkable Dot (22, 12),
    NotWalkable VERTICAL (23, 12),
    Walkable Empty (24, 12),
    Walkable Empty (25, 12),
    Walkable Empty (26, 12),
    Walkable Empty (27, 12),
    Walkable Empty (28, 12),
    Walkable Empty (1, 13),
    Walkable Empty (2, 13),
    Walkable Empty (3, 13),
    Walkable Empty (4, 13),
    Walkable Empty (5, 13),
    NotWalkable VERTICAL (6, 13),
    Walkable Dot (7, 13),
    NotWalkable HORIZONTAL (8, 13),
    NotWalkable HORIZONTAL (9, 13),
    Walkable Empty (10, 13),
    Walkable Empty (11, 13),
    NotWalkable HORIZONTAL (12, 13),
    NotWalkable HORIZONTAL (13, 13),
    Walkable DOOR (14, 13),
    Walkable DOOR (15, 13),
    NotWalkable HORIZONTAL (16, 13),
    NotWalkable HORIZONTAL (17, 13),
    Walkable Empty (18, 13),
    Walkable Empty (19, 13),
    NotWalkable HORIZONTAL (20, 13),
    NotWalkable HORIZONTAL (21, 13),
    Walkable Dot (22, 13),
    NotWalkable VERTICAL (23, 13),
    Walkable Empty (24, 13),
    Walkable Empty (25, 13),
    Walkable Empty (26, 13),
    Walkable Empty (27, 13),
    Walkable Empty (28, 13),
    Walkable Empty (1, 14),
    Walkable Empty (2, 14),
    Walkable Empty (3, 14),
    Walkable Empty (4, 14),
    Walkable Empty (5, 14),
    NotWalkable VERTICAL (6, 14),
    Walkable Dot (7, 14),
    Walkable Empty (8, 14),
    Walkable Empty (9, 14),
    Walkable Empty (10, 14),
    Walkable Empty (11, 14),
    NotWalkable VERTICAL (12, 14),
    Walkable Empty (13, 14),
    Walkable Empty (14, 14),
    Walkable Empty (15, 14),
    Walkable Empty (16, 14),
    NotWalkable VERTICAL (17, 14),
    Walkable Empty (18, 14),
    Walkable Empty (19, 14),
    Walkable Empty (20, 14),
    Walkable Empty (21, 14),
    Walkable Dot (22, 14),
    NotWalkable VERTICAL (23, 14),
    Walkable Empty (24, 14),
    Walkable Empty (25, 14),
    Walkable Empty (26, 14),
    Walkable Empty (27, 14),
    Walkable Empty (28, 14),
    Walkable Empty (1, 15),
    Walkable Empty (2, 15),
    Walkable Empty (3, 15),
    Walkable Empty (4, 15),
    Walkable Empty (5, 15),
    NotWalkable VERTICAL (6, 15),
    Walkable Dot (7, 15),
    NotWalkable HORIZONTAL (8, 15),
    NotWalkable HORIZONTAL (9, 15),
    Walkable Empty (10, 15),
    Walkable Empty (11, 15),
    NotWalkable VERTICAL (12, 15),
    Walkable Empty (13, 15),
    Walkable Empty (14, 15),
    Walkable Empty (15, 15),
    Walkable Empty (16, 15),
    NotWalkable VERTICAL   (17, 15),
    Walkable Empty (18, 15),
    Walkable Empty (19, 15),
    NotWalkable HORIZONTAL (20, 15),
    NotWalkable HORIZONTAL (21, 15),
    Walkable Dot (22, 15),
    NotWalkable VERTICAL (23, 15),
    Walkable Empty (24, 15),
    Walkable Empty (25, 15),
    Walkable Empty (26, 15),
    Walkable Empty (27, 15),
    Walkable Empty (28, 15),
    Walkable Empty (1, 16),
    Walkable Empty (2, 16),
    Walkable Empty (3, 16),
    Walkable Empty (4, 16),
    Walkable Empty (5, 16),
    NotWalkable VERTICAL (6, 16),
    Walkable Dot (7, 16),
    NotWalkable HORIZONTAL (8, 16),
    NotWalkable HORIZONTAL (9, 16),
    Walkable Empty (10, 16),
    Walkable Empty (11, 16),
    NotWalkable HORIZONTAL (12, 16),
    NotWalkable HORIZONTAL (13, 16),
    NotWalkable HORIZONTAL (14, 16),
    NotWalkable HORIZONTAL (15, 16),
    NotWalkable HORIZONTAL (16, 16),
    NotWalkable HORIZONTAL (17, 16),
    Walkable Empty (18, 16),
    Walkable Empty (19, 16),
    NotWalkable HORIZONTAL (20, 16),
    NotWalkable HORIZONTAL (21, 16),
    Walkable Dot (22, 16),
    NotWalkable VERTICAL (23, 16),
    Walkable Empty (24, 16),
    Walkable Empty (25, 16),
    Walkable Empty (26, 16),
    Walkable Empty (27, 16),
    Walkable Empty (28, 16),
    Walkable Empty (1, 17),
    Walkable Empty (2, 17),
    Walkable Empty (3, 17),
    Walkable Empty (4, 17),
    Walkable Empty (5, 17),
    NotWalkable VERTICAL (6, 17),
    Walkable Dot (7, 17),
    NotWalkable HORIZONTAL (8, 17),
    NotWalkable HORIZONTAL (9, 17),
    Walkable Empty (10, 17),
    Walkable Empty (11, 17),
    Walkable Empty (12, 17),
    Walkable Empty (13, 17),
    Walkable Empty (14, 17),
    Walkable Empty (15, 17),
    Walkable Empty (16, 17),
    Walkable Empty (17, 17),
    Walkable Empty (18, 17),
    Walkable Empty (19, 17),
    NotWalkable HORIZONTAL (20, 17),
    NotWalkable HORIZONTAL (21, 17),
    Walkable Dot (22, 17),
    NotWalkable VERTICAL (23, 17),
    Walkable Empty (24, 17),
    Walkable Empty (25, 17),
    Walkable Empty (26, 17),
    Walkable Empty (27, 17),
    Walkable Empty (28, 17),
    Walkable Empty (1, 18),
    Walkable Empty (2, 18),
    Walkable Empty (3, 18),
    Walkable Empty (4, 18),
    Walkable Empty (5, 18),
    NotWalkable VERTICAL (6, 18),
    Walkable Dot (7, 18),
    NotWalkable HORIZONTAL (8, 18),
    NotWalkable HORIZONTAL (9, 18),
    Walkable Empty (10, 18),
    NotWalkable HORIZONTAL (11, 18),
    NotWalkable HORIZONTAL (12, 18),
    NotWalkable HORIZONTAL (13, 18),
    NotWalkable HORIZONTAL (14, 18),
    NotWalkable HORIZONTAL (15, 18),
    NotWalkable HORIZONTAL (16, 18),
    NotWalkable HORIZONTAL (17, 18),
    NotWalkable HORIZONTAL (18, 18),
    Walkable Empty (19, 18),
    NotWalkable HORIZONTAL (20, 18),
    NotWalkable HORIZONTAL (21, 18),
    Walkable Dot (22, 18),
    NotWalkable VERTICAL (23, 18),
    Walkable Empty (24, 18),
    Walkable Empty (25, 18),
    Walkable Empty (26, 18),
    Walkable Empty (27, 18),
    Walkable Empty (28, 18),
    NotWalkable HORIZONTAL (1, 19),
    NotWalkable HORIZONTAL (2, 19),
    NotWalkable HORIZONTAL (3, 19),
    NotWalkable HORIZONTAL (4, 19),
    NotWalkable HORIZONTAL (5, 19),
    NotWalkable HORIZONTAL (6, 19),
    Walkable Dot (7, 19),
    NotWalkable HORIZONTAL (8, 19),
    NotWalkable HORIZONTAL (9, 19),
    Walkable Empty (10, 19),
    NotWalkable HORIZONTAL (11, 19),
    NotWalkable HORIZONTAL (12, 19),
    NotWalkable HORIZONTAL (13, 19),
    NotWalkable HORIZONTAL (14, 19),
    NotWalkable HORIZONTAL (15, 19),
    NotWalkable HORIZONTAL (16, 19),
    NotWalkable HORIZONTAL (17, 19),
    NotWalkable HORIZONTAL (18, 19),
    Walkable Empty (19, 19),
    NotWalkable HORIZONTAL (20, 19),
    NotWalkable HORIZONTAL (21, 19),
    Walkable Dot (22, 19),
    NotWalkable HORIZONTAL (23, 19),
    NotWalkable HORIZONTAL (24, 19),
    NotWalkable HORIZONTAL (25, 19),
    NotWalkable HORIZONTAL (26, 19),
    NotWalkable HORIZONTAL (27, 19),
    NotWalkable HORIZONTAL (28, 19),
    NotWalkable VERTICAL (1, 20),
    Walkable Dot (2, 20),
    Walkable Dot (3, 20),
    Walkable Dot (4, 20),
    Walkable Dot (5, 20),
    Walkable Dot (6, 20),
    Walkable Dot (7, 20),
    Walkable Dot (8, 20),
    Walkable Dot (9, 20),
    Walkable Dot (10, 20),
    Walkable Dot (11, 20),
    Walkable Dot (12, 20),
    Walkable Dot (13, 20),
    NotWalkable VERTICAL (14, 20),
    NotWalkable VERTICAL (15, 20),
    Walkable Dot (16, 20),
    Walkable Dot (17, 20),
    Walkable Dot (18, 20),
    Walkable Dot (19, 20),
    Walkable Dot (20, 20),
    Walkable Dot (21, 20),
    Walkable Dot (22, 20),
    Walkable Dot (23, 20),
    Walkable Dot (24, 20),
    Walkable Dot (25, 20),
    Walkable Dot (26, 20),
    Walkable Dot (27, 20),
    NotWalkable VERTICAL (28, 20),
    NotWalkable VERTICAL (1, 21),
    Walkable Dot (2, 21),
    NotWalkable HORIZONTAL (3, 21),
    NotWalkable HORIZONTAL (4, 21),
    NotWalkable HORIZONTAL (5, 21),
    NotWalkable HORIZONTAL (6, 21),
    Walkable Dot (7, 21),
    NotWalkable HORIZONTAL (8, 21),
    NotWalkable HORIZONTAL (9, 21),
    NotWalkable HORIZONTAL (10, 21),
    NotWalkable HORIZONTAL (11, 21),
    NotWalkable HORIZONTAL (12, 21),
    Walkable Dot (13, 21),
    NotWalkable VERTICAL (14, 21),
    NotWalkable VERTICAL (15, 21),
    Walkable Dot (16, 21),
    NotWalkable HORIZONTAL (17, 21),
    NotWalkable HORIZONTAL (18, 21),
    NotWalkable HORIZONTAL (19, 21),
    NotWalkable HORIZONTAL (20, 21),
    NotWalkable HORIZONTAL (21, 21),
    Walkable Dot (22, 21),
    NotWalkable HORIZONTAL (23, 21),
    NotWalkable HORIZONTAL (24, 21),
    NotWalkable HORIZONTAL (25, 21),
    NotWalkable HORIZONTAL (26, 21),
    Walkable Dot (27, 21),
    NotWalkable VERTICAL (28, 21),
    NotWalkable VERTICAL (1, 22),
    Walkable Dot (2, 22),
    NotWalkable HORIZONTAL (3, 22),
    NotWalkable HORIZONTAL (4, 22),
    NotWalkable HORIZONTAL (5, 22),
    NotWalkable HORIZONTAL (6, 22),
    Walkable Dot (7, 22),
    NotWalkable HORIZONTAL (8, 22),
    NotWalkable HORIZONTAL (9, 22),
    NotWalkable HORIZONTAL (10, 22),
    NotWalkable HORIZONTAL (11, 22),
    NotWalkable HORIZONTAL (12, 22),
    Walkable Dot (13, 22),
    NotWalkable VERTICAL (14, 22),
    NotWalkable VERTICAL (15, 22),
    Walkable Dot (16, 22),
    NotWalkable HORIZONTAL (17, 22),
    NotWalkable HORIZONTAL (18, 22),
    NotWalkable HORIZONTAL (19, 22),
    NotWalkable HORIZONTAL (20, 22),
    NotWalkable HORIZONTAL (21, 22),
    Walkable Dot (22, 22),
    NotWalkable HORIZONTAL (23, 22),
    NotWalkable HORIZONTAL (24, 22),
    NotWalkable HORIZONTAL (25, 22),
    NotWalkable HORIZONTAL (26, 22),
    Walkable Dot (27, 22),
    NotWalkable VERTICAL (28, 22),
    NotWalkable VERTICAL (1, 23),
    Walkable Coin (2, 23),
    Walkable Dot (3, 23),
    Walkable Dot (4, 23),
    NotWalkable HORIZONTAL (5, 23),
    NotWalkable HORIZONTAL (6, 23),
    Walkable Dot (7, 23),
    Walkable Dot (8, 23),
    Walkable Dot (9, 23),
    Walkable Dot (10, 23),
    Walkable Dot (11, 23),
    Walkable Dot (12, 23),
    Walkable Dot (13, 23),
    Walkable Empty (14, 23),
    Walkable Empty (15, 23),
    Walkable Dot (16, 23),
    Walkable Dot (17, 23),
    Walkable Dot (18, 23),
    Walkable Dot (19, 23),
    Walkable Dot (20, 23),
    Walkable Dot (21, 23),
    Walkable Dot (22, 23),
    NotWalkable HORIZONTAL (23, 23),
    NotWalkable HORIZONTAL (24, 23),
    Walkable Dot (25, 23),
    Walkable Dot (26, 23),
    Walkable Coin (27, 23),
    NotWalkable VERTICAL (28, 23),
    NotWalkable VERTICAL (1, 24),
    NotWalkable HORIZONTAL (2, 24),
    NotWalkable HORIZONTAL (3, 24),
    Walkable Dot (4, 24),
    NotWalkable HORIZONTAL (5, 24),
    NotWalkable HORIZONTAL (6, 24),
    Walkable Dot (7, 24),
    NotWalkable HORIZONTAL (8, 24),
    NotWalkable HORIZONTAL (9, 24),
    Walkable Dot (10, 24),
    NotWalkable HORIZONTAL (11, 24),
    NotWalkable HORIZONTAL (12, 24),
    NotWalkable HORIZONTAL (13, 24),
    NotWalkable HORIZONTAL (14, 24),
    NotWalkable HORIZONTAL (15, 24),
    NotWalkable HORIZONTAL (16, 24),
    NotWalkable HORIZONTAL (17, 24),
    NotWalkable HORIZONTAL (18, 24),
    Walkable Dot (19, 24),
    NotWalkable HORIZONTAL (20, 24),
    NotWalkable HORIZONTAL (21, 24),
    Walkable Dot (22, 24),
    NotWalkable HORIZONTAL (23, 24),
    NotWalkable HORIZONTAL (24, 24),
    Walkable Dot (25, 24),
    NotWalkable HORIZONTAL (26, 24),
    NotWalkable HORIZONTAL (27, 24),
    NotWalkable VERTICAL (28, 24),
    NotWalkable VERTICAL (1, 25),
    NotWalkable HORIZONTAL (2, 25),
    NotWalkable HORIZONTAL (3, 25),
    Walkable Dot (4, 25),
    NotWalkable HORIZONTAL (5, 25),
    NotWalkable HORIZONTAL (6, 25),
    Walkable Dot (7, 25),
    NotWalkable HORIZONTAL (8, 25),
    NotWalkable HORIZONTAL (9, 25),
    Walkable Dot (10, 25),
    NotWalkable HORIZONTAL (11, 25),
    NotWalkable HORIZONTAL (12, 25),
    NotWalkable HORIZONTAL (13, 25),
    NotWalkable HORIZONTAL (14, 25),
    NotWalkable HORIZONTAL (15, 25),
    NotWalkable HORIZONTAL (16, 25),
    NotWalkable HORIZONTAL (17, 25),
    NotWalkable HORIZONTAL (18, 25),
    Walkable Dot (19, 25),
    NotWalkable HORIZONTAL (20, 25),
    NotWalkable HORIZONTAL (21, 25),
    Walkable Dot (22, 25),
    NotWalkable HORIZONTAL (23, 25),
    NotWalkable HORIZONTAL (24, 25),
    Walkable Dot (25, 25),
    NotWalkable HORIZONTAL (26, 25),
    NotWalkable HORIZONTAL (27, 25),
    NotWalkable VERTICAL (28, 25),
    NotWalkable VERTICAL (1, 26),
    Walkable Dot (2, 26),
    Walkable Dot (3, 26),
    Walkable Dot (4, 26),
    Walkable Dot (5, 26),
    Walkable Dot (6, 26),
    Walkable Dot (7, 26),
    NotWalkable HORIZONTAL (8, 26),
    NotWalkable HORIZONTAL (9, 26),
    Walkable Dot (10, 26),
    Walkable Dot (11, 26),
    Walkable Dot (12, 26),
    Walkable Dot (13, 26),
    NotWalkable HORIZONTAL (14, 26),
    NotWalkable HORIZONTAL (15, 26),
    Walkable Dot (16, 26),
    Walkable Dot (17, 26),
    Walkable Dot (18, 26),
    Walkable Dot (19, 26),
    NotWalkable HORIZONTAL (20, 26),
    NotWalkable HORIZONTAL (21, 26),
    Walkable Dot (22, 26),
    Walkable Dot (23, 26),
    Walkable Dot (24, 26),
    Walkable Dot (25, 26),
    Walkable Dot (26, 26),
    Walkable Dot (27, 26),
    NotWalkable VERTICAL (28, 26),
    NotWalkable VERTICAL (1, 27),
    Walkable Dot (2, 27),
    NotWalkable HORIZONTAL (3, 27),
    NotWalkable HORIZONTAL (4, 27),
    NotWalkable HORIZONTAL (5, 27),
    NotWalkable HORIZONTAL (6, 27),
    NotWalkable HORIZONTAL (7, 27),
    NotWalkable HORIZONTAL (8, 27),
    NotWalkable HORIZONTAL (9, 27),
    NotWalkable HORIZONTAL (10, 27),
    NotWalkable HORIZONTAL (11, 27),
    NotWalkable HORIZONTAL (12, 27),
    Walkable Dot (13, 27),
    NotWalkable HORIZONTAL (14, 27),
    NotWalkable HORIZONTAL (15, 27),
    Walkable Dot (16, 27),
    NotWalkable HORIZONTAL (17, 27),
    NotWalkable HORIZONTAL (18, 27),
    NotWalkable HORIZONTAL (19, 27),
    NotWalkable HORIZONTAL (20, 27),
    NotWalkable HORIZONTAL (21, 27),
    NotWalkable HORIZONTAL (22, 27),
    NotWalkable HORIZONTAL (23, 27),
    NotWalkable HORIZONTAL (24, 27),
    NotWalkable HORIZONTAL (25, 27),
    NotWalkable HORIZONTAL (26, 27),
    Walkable Dot (27, 27),
    NotWalkable VERTICAL (28, 27),
    NotWalkable VERTICAL (1, 28),
    Walkable Dot (2, 28),
    NotWalkable HORIZONTAL (3, 28),
    NotWalkable HORIZONTAL (4, 28),
    NotWalkable HORIZONTAL (5, 28),
    NotWalkable HORIZONTAL (6, 28),
    NotWalkable HORIZONTAL (7, 28),
    NotWalkable HORIZONTAL (8, 28),
    NotWalkable HORIZONTAL (9, 28),
    NotWalkable HORIZONTAL (10, 28),
    NotWalkable HORIZONTAL (11, 28),
    NotWalkable HORIZONTAL (12, 28),
    Walkable Dot (13, 28),
    NotWalkable HORIZONTAL (14, 28),
    NotWalkable HORIZONTAL (15, 28),
    Walkable Dot (16, 28),
    NotWalkable HORIZONTAL (17, 28),
    NotWalkable HORIZONTAL (18, 28),
    NotWalkable HORIZONTAL (19, 28),
    NotWalkable HORIZONTAL (20, 28),
    NotWalkable HORIZONTAL (21, 28),
    NotWalkable HORIZONTAL (22, 28),
    NotWalkable HORIZONTAL (23, 28),
    NotWalkable HORIZONTAL (24, 28),
    NotWalkable HORIZONTAL (25, 28),
    NotWalkable HORIZONTAL (26, 28),
    Walkable Dot (27, 28),
    NotWalkable VERTICAL (28, 28),
    NotWalkable VERTICAL (1, 29),
    Walkable Dot (2, 29),
    Walkable Dot (3, 29),
    Walkable Dot (4, 29),
    Walkable Dot (5, 29),
    Walkable Dot (6, 29),
    Walkable Dot (7, 29),
    Walkable Dot (8, 29),
    Walkable Dot (9, 29),
    Walkable Dot (10, 29),
    Walkable Dot (11, 29),
    Walkable Dot (12, 29),
    Walkable Dot (13, 29),
    Walkable Dot (14, 29),
    Walkable Dot (15, 29),
    Walkable Dot (16, 29),
    Walkable Dot (17, 29),
    Walkable Dot (18, 29),
    Walkable Dot (19, 29),
    Walkable Dot (20, 29),
    Walkable Dot (21, 29),
    Walkable Dot (22, 29),
    Walkable Dot (23, 29),
    Walkable Dot (24, 29),
    Walkable Dot (25, 29),
    Walkable Dot (26, 29),
    Walkable Dot (27, 29),
    NotWalkable VERTICAL (28, 29),
    NotWalkable HORIZONTAL (1, 30),
    NotWalkable HORIZONTAL (2, 30),
    NotWalkable HORIZONTAL (3, 30),
    NotWalkable HORIZONTAL (4, 30),
    NotWalkable HORIZONTAL (5, 30),
    NotWalkable HORIZONTAL (6, 30),
    NotWalkable HORIZONTAL (7, 30),
    NotWalkable HORIZONTAL (8, 30),
    NotWalkable HORIZONTAL (9, 30),
    NotWalkable HORIZONTAL (10, 30),
    NotWalkable HORIZONTAL (11, 30),
    NotWalkable HORIZONTAL (12, 30),
    NotWalkable HORIZONTAL (13, 30),
    NotWalkable HORIZONTAL (14, 30),
    NotWalkable HORIZONTAL (15, 30),
    NotWalkable HORIZONTAL (16, 30),
    NotWalkable HORIZONTAL (17, 30),
    NotWalkable HORIZONTAL (18, 30),
    NotWalkable HORIZONTAL (19, 30),
    NotWalkable HORIZONTAL (20, 30),
    NotWalkable HORIZONTAL (21, 30),
    NotWalkable HORIZONTAL (22, 30),
    NotWalkable HORIZONTAL (23, 30),
    NotWalkable HORIZONTAL (24, 30),
    NotWalkable HORIZONTAL (25, 30),
    NotWalkable HORIZONTAL (26, 30),
    NotWalkable HORIZONTAL (27, 30),
    NotWalkable HORIZONTAL (28, 30)
  ]
