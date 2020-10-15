module Logic.Ghost where
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate

updateGhost :: Player -> Player
updateGhost ghost = ghost

updateDirection :: Player -> Direction -> Player
updateDirection (PacMan position score direction) d = PacMan position score d

setPlayerDirection :: Direction -> GameState -> GameState
setPlayerDirection d game = game {player = updateDirection (player game) d}
