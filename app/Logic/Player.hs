module Logic.Player where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model




-- Updating the player
updateDirection :: Player -> (Direction) -> Player
updateDirection (PacMan position score (cur, next)) d = PacMan position score (cur,d)

-- * Adjusting the player direction
overrideDirection :: Player -> (Direction, Direction) -> Player
overrideDirection (PacMan position score dir) ds = PacMan position score ds

-- * Storing the player direction to the GameState
setPlayerDirection :: Direction -> GameState -> GameState
setPlayerDirection d game = game {player = updateDirection (player game) d}

-- * Storing the adjusted player to the GameState
updatePosition :: GameState -> GameState
updatePosition gstate = gstate {player = updatePlayerPosition gstate}

-- * Getting a new player instance with an updated position
updatePlayerPosition :: GameState -> Player
updatePlayerPosition gstate = p2
  where (pos, newDir) = getNewPlayerPosition gstate
        p             = modifyPlayerPosition pos $ player gstate
        p2            = overrideDirection p newDir

-- * Getting the new possible position for a player. If the second direction can be walked, do this and discard the first direction
getNewPlayerPosition :: GameState -> ((Int, Int), (Direction, Direction))
getNewPlayerPosition gstate = let pos = position (player gstate) in
                                case (direction (player gstate) ) of
                                  (cur, NOTHING)  -> let nPos = calculateNextPosition pos cur in
                                                      if positionWalkable nPos gstate then (nPos, (cur,NOTHING)) else (pos, (cur,NOTHING) )
                                  (cur,next)      -> let
                                                      pos = position (player gstate)
                                                      nPos = calculateNextPosition pos next
                                                      nPos2 = calculateNextPosition pos cur in
                                                      if positionWalkable nPos gstate then (nPos, (next,NOTHING)) else
                                                        if positionWalkable nPos2 gstate then (nPos2, (cur,next)) else (pos, (cur,next) )


-- * Setting the new position for the player
modifyPlayerPosition :: (Int, Int) -> Player -> Player
modifyPlayerPosition (x, y) (PacMan position score direction) = PacMan (x, y) score direction


-- * Based on the direction a player is set to go, we calculate the vector for the tile position
calculateNextPosition :: (Int, Int) -> Direction -> (Int, Int)
calculateNextPosition (x, y)  UP = (x   , y -1)
calculateNextPosition (x, y)  DOWN   = (x   , y +1)
calculateNextPosition (x, y)  LEFT   = (x -1, y   )
calculateNextPosition (x, y)  RIGHT  = (x +1, y   )
calculateNextPosition (x, y)  _      = (1,1) -- show place for not walkable

-- * Updating the player specifically.
updatePlayer :: GameState -> GameState
updatePlayer = updatePosition


-- * Checking if the position that a player wants to go is accessible.
positionWalkable :: (Int, Int) -> GameState -> Bool
positionWalkable position gstate = isWalkable $ getTileByPosition gstate position
