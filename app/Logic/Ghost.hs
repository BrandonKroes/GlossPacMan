module Logic.Ghost where
import Model
import Logic.PathFinding
import Logic.Player

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = map (updateGhost gstate) $ ghosts gstate}

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

