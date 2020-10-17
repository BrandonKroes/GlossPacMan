module Logic.Ghost where
import Model
import Logic.PathFinding
import Logic.Player

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = foldr f [] (ghosts gstate)}
                        where f x r = (updateGhost gstate x) : r

updateGhost :: GameState -> Player -> Player
updateGhost gstate (Ghost gPos RED mod) = Ghost loc RED mod
    where loc = case findPath gstate gPos (position (player gstate )) of
                Just (steps, (x:xs)) -> x
                _                    -> gPos

updateGhost gstate (Ghost gPos PINK mod) = Ghost loc PINK mod
    where loc = case findPath gstate gPos newPlayerPos of
                Just (steps, (x:xs)) -> x
                _                    -> gPos
          newPlayerPos = position (player (updatePosition (updatePosition gstate) ) )  -- update the player position with two steps
updateGhost gstate ghost = ghost

updateDirection :: Player -> Direction -> Player
updateDirection (PacMan position score direction) d = PacMan position score d

setPlayerDirection :: Direction -> GameState -> GameState
setPlayerDirection d game = game {player = updateDirection (player game) d}
