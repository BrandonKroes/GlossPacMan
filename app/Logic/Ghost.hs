module Logic.Ghost where
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = map updateGhost $ ghosts gstate}

updateGhost :: Player -> Player
updateGhost ghost = ghost
