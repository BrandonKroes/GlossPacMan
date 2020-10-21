module Logic.Conditions where

import Model
-- Conditions takes care of deciding when the game ends & conditional logic that requires all gamestate variables set


conditions::GameState->GameState
conditions gstate = playerEatingFrightenedGhost $ detectGhostOnPlayer $  allDotsGone gstate

allDotsGone::GameState->GameState
allDotsGone gstate = gstate {runningState = detectDotsGone gstate $ consumablesLeft gstate}


detectDotsGone::GameState->Int-> RunningState
detectDotsGone gstate dots | RUNNING /= (runningState gstate) = (runningState gstate)
                           | dots <= 0 = WON
                           | otherwise = RUNNING


runningStateGhostOnPlayer::GameState->Ghosts->(Int, Int) -> RunningState
runningStateGhostOnPlayer gstate g pp
                    | RUNNING /= (runningState gstate) = (runningState gstate)
                    | areDeadlyGhostsOnPlayer g pp == False = LOST
                    | otherwise = RUNNING


playerEatingFrightenedGhost::GameState->GameState
playerEatingFrightenedGhost gstate | null frightenedGhosts = gstate
                                   | otherwise = gstate {ghosts=newGhosts}
  where
    playerPosition = getPlayerPosition $ player gstate
    g = ghosts gstate
    frightenedGhosts = filter (samePosition playerPosition) $ filter (isStateGhost Frightened) g
    notFrightenedGhosts = filter (notSamePosition playerPosition) g
    newState = Retreat
    t = time gstate
    newGhosts = notFrightenedGhosts ++ (setGhostsToState newState frightenedGhosts t)



areDeadlyGhostsOnPlayer::Ghosts-> (Int, Int)-> Bool
areDeadlyGhostsOnPlayer ghosts pp = null $ filter (\x ->  x == pp) $ getDeadlyGhostsPosition ghosts

detectGhostOnPlayer::GameState->GameState
detectGhostOnPlayer gstate = gstate {runningState = runningStateGhostOnPlayer gstate (ghosts gstate) (getPlayerPosition $ player gstate)}
