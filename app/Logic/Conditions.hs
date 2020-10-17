module Logic.Conditions where

import Model
-- Conditions takes care of deciding when the game ends & conditional logic that requires all gamestate variables set


conditions::GameState->GameState
conditions gstate = detectGhostOnPlayer $ allDotsGone gstate

allDotsGone::GameState->GameState
allDotsGone gstate = gstate {runningState = detectDotsGone gstate $ dotsLeft gstate}


detectDotsGone::GameState->Int-> RunningState
detectDotsGone gstate dots | RUNNING /= (runningState gstate) = (runningState gstate)
                           | dots <= 0 = WON
                           | otherwise = RUNNING


runningStateGhostOnPlayer::GameState->Ghosts->(Int, Int) -> RunningState
runningStateGhostOnPlayer gstate g pp
                    | RUNNING /= (runningState gstate) = (runningState gstate)
                    | areGhostsOnPlayer g pp == False = LOST
                    | otherwise = RUNNING

areGhostsOnPlayer::Ghosts-> (Int, Int)-> Bool
areGhostsOnPlayer ghosts pp = null $ filter (\x ->  x == pp) $ getGhostsPosition ghosts

detectGhostOnPlayer::GameState->GameState
detectGhostOnPlayer gstate = gstate {runningState = runningStateGhostOnPlayer gstate (ghosts gstate) (getPlayerPosition $ player gstate)}
