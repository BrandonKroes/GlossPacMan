module Logic.Player where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model




-- Updating the player

updateDirection :: Player -> Direction -> Player
updateDirection (PacMan position score direction) d = PacMan position score d

setPlayerDirection :: Direction -> GameState -> GameState
setPlayerDirection d game = game {player = updateDirection (player game) d}


updatePosition :: GameState -> GameState
updatePosition gstate = gstate {player = updatePlayerPosition gstate}

updatePlayerPosition :: GameState -> Player
updatePlayerPosition gstate = modifyPlayerPosition (getNewPlayerPosition gstate) $ player gstate

getNewPlayerPosition :: GameState -> (Int, Int)
getNewPlayerPosition gstate
  | walkable == True = nPosition
  | walkable == False = oPosition
  where
    oPosition = position $ player gstate
    nPosition = calculateNextPlayerPosition $ player gstate
    walkable = positionWalkable nPosition gstate

modifyPlayerPosition :: (Int, Int) -> Player -> Player
modifyPlayerPosition (x, y) (PacMan position score direction) = PacMan (x, y) score direction

calculateNextPlayerPosition :: Player -> (Int, Int)
calculateNextPlayerPosition (PacMan (x, y) score direction)
  | direction == UP     = (x   , y -1)
  | direction == DOWN   = (x   , y +1)
  | direction == LEFT   = (x -1, y   )
  | direction == RIGHT  = (x +1, y   )

updatePlayer :: GameState -> GameState
updatePlayer = updatePosition

positionWalkable :: (Int, Int) -> GameState -> Bool
positionWalkable position gstate = isWalkable $ getTileByPosition gstate position
