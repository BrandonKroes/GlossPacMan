module Logic.Conditions where

import Data.List

import Model
-- Conditions takes care of deciding when the game ends & conditional logic that requires all gamestate variables set


conditions::GameState->GameState
conditions gstate = playerEatingFrightenedGhost $ detectGhostOnPlayer $  allDotsGone gstate

allDotsGone::GameState->GameState
allDotsGone gstate = gstate {runningState = detectDotsGone gstate $ consumablesLeft gstate}


detectDotsGone::GameState->Int-> RunningState
detectDotsGone gstate dots | RUNNING /= (runningState gstate) = (runningState gstate)
                           | dots <= 0 = let x = updateHighscore gstate "HighScore/highscore.txt"  in
                                            WON
                           | otherwise = RUNNING

placeScoreInList :: (Float, Float) -> [[Float]] -> [[Float]]
placeScoreInList (score, time) []         = [[score, time]]
placeScoreInList (score, time) (x@(s:t:rs):xs) | score < s = x : placeScoreInList (score, time) xs
                                          | score > s = [score, time] : x : xs
                                          | otherwise = case time < t of 
                                                          True -> [score, time] : x : xs
                                                          _    -> x : placeScoreInList (score,time) xs

readToFloat (x:xs:[]) = (x ::Float) : (xs ::Float) : []

updateHighscore :: GameState -> FilePath -> IO()
updateHighscore gstate fileName = do 
                                  highscoreStr <- readFile fileName 
                                  let score1 = fromIntegral  (score (player gstate))
                                      time1  = time gstate
                                      lscores = lines highscoreStr
                                      llscore = map words lscores
                                      rscore = map (map read) llscore 
                                      iscore = map readToFloat rscore
                                      newList = placeScoreInList (score1,time1) iscore 
                                      strList = map (map show) newList
                                      newStr = unlines $ map (intercalate " ") strList
                                  writeFile fileName newStr 


runningStateGhostOnPlayer::GameState->Ghosts->(Int, Int) -> RunningState
runningStateGhostOnPlayer gstate g pp
                    | RUNNING /= (runningState gstate) = (runningState gstate)
                    | areDeadlyGhostsOnPlayer g pp == False = let x = updateHighscore gstate "HighScore/highscore.txt" in LOST
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
