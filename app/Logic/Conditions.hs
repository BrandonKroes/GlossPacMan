module Logic.Conditions where

import Data.List
import Model
-- Conditions takes care of deciding when the game ends & conditional logic that requires all gamestate variables set


conditions::GameState-> IO GameState
conditions gstate = do adg <- allDotsGone gstate
                       dgop <- detectGhostOnPlayer adg
                       return (playerEatingFrightenedGhost dgop)

allDotsGone::GameState-> IO GameState
allDotsGone gstate = do ddg <- detectDotsGone gstate $ consumablesLeft gstate
                        return (gstate {runningState = ddg})


detectDotsGone::GameState->Int-> IO RunningState
detectDotsGone gstate dots | RUNNING /= (runningState gstate) = return (runningState gstate)
                           | dots <= 0 = do updateHighscore gstate "HighScore/highscore.txt"
                                            return WON
                           | otherwise = return RUNNING

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

runningStateGhostOnPlayer::GameState->Ghosts->(Int, Int) -> IO RunningState
runningStateGhostOnPlayer gstate g pp
                    | RUNNING /= (runningState gstate) = return (runningState gstate)
                    | not (areDeadlyGhostsOnPlayer g pp) = do updateHighscore gstate "HighScore/highscore.txt"
                                                              return LOST
                    | otherwise = return RUNNING


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

detectGhostOnPlayer::GameState -> IO GameState
detectGhostOnPlayer gstate = do rsogp <- runningStateGhostOnPlayer gstate (ghosts gstate) (getPlayerPosition $ player gstate)
                                return (gstate {runningState = rsogp})
