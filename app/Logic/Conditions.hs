module Logic.Conditions where

import Data.List
import System.IO.Strict
import Model


-- * Conditions takes care of deciding when the game ends & conditional logic that requires all gamestate variables set
conditions::GameState-> IO GameState
conditions gstate = do adg <- allDotsGone gstate
                       dgop <- detectGhostOnPlayer adg
                       return (playerEatingFrightenedGhost dgop)


-- * Updates the game state to a possible new gamestate
allDotsGone::GameState-> IO GameState
allDotsGone gstate = do ddg <- detectDotsGone gstate $ consumablesLeft gstate
                        return (gstate {runningState = ddg})

-- Check if dots are 0 or less. If so, update the highscore and return WOn, else do not much
detectDotsGone::GameState->Int-> IO RunningState
detectDotsGone gstate dots | (runningState gstate) /= RUNNING = return (runningState gstate)
                           | dots <= 0 = do updateHighscore gstate "HighScore/highscore.txt"
                                            return WON
                           | otherwise = return RUNNING

-- update the score in the list of scores. The higher the score and lower the time, the better. Score is leading, timing is second
placeScoreInList :: (Float, Float) -> [[Float]] -> [[Float]]
placeScoreInList (score, time) []         = [[score, time]]
placeScoreInList (score, time) (x@(s:t:rs):xs) | score < s = x : placeScoreInList (score, time) xs
                                               | score > s = [score, time] : x : xs
                                               | otherwise = case time < t of
                                                            True -> [score, time] : x : xs
                                                            _    -> x : placeScoreInList (score,time) xs

-- Sometimes a read is not fully converted to float. In this function it is done explicitly
readToFloat :: [Float] -> [Float]
readToFloat = map (\x -> (x ::Float))

-- for each element in the list, add the first element of the first list to the list eg. [1,2] [[3,4],[5,6]] = [[1,3,4], [2,5,6]]
zip_ :: [a] -> [[a]] -> [[a]]
zip_ _      []    = []
zip_ []     ys    = ys
zip_ (x:xs) (y:ys) = (x:y) : zip_ xs ys

-- read the current highscore from file, include the current score into it and write it back to the file
updateHighscore :: GameState -> FilePath -> IO()
updateHighscore gstate fileName = do
                                  highscoreStr <- System.IO.Strict.readFile fileName
                                  let score0 = fromIntegral (score (player gstate) + (((consumablesTotal gstate) - (consumablesLeft gstate)) * 100)) -- get raw score
                                      score1 = (fromInteger $ round $ score0 * (10^2)) / (10.0^^2) -- round to two decimals
                                      time1 = (fromInteger $ round $ (time gstate) * (10^2)) / (10.0^^2) -- round to two decimals
                                      (header : lscores) = if highscoreStr == [] then ["Nr \t\t Score \t\t Time"] else  lines highscoreStr -- put all lines in one listelement
                                      llscore = map (drop 1) $ map words lscores --split string in list of strings and remove number
                                      rscore = map (map read) llscore -- switch to Integer datatype
                                      iscore = map readToFloat rscore
                                      newList0 = placeScoreInList (score1,time1) iscore
                                      newList = zip_ [1..] newList0
                                      newStr = unlines $ map (intercalate "\t\t") $ map (map show) newList
                                  writeFile fileName (header ++ "\n" ++ newStr)

-- Check if a ghost overlaps with the player. If so, the game is lost and the highscore list will be updated
runningStateGhostOnPlayer::GameState->Ghosts->(Int, Int) -> IO RunningState
runningStateGhostOnPlayer gstate g pp
                    | (runningState gstate) /= RUNNING = return (runningState gstate)
                    | not (areDeadlyGhostsOnPlayer g pp) = do updateHighscore gstate "HighScore/highscore.txt"
                                                              return LOST
                    | otherwise = return RUNNING

-- Check if there is a frightened ghost on the same position as PacMan. If so, update the score, ghost and player
playerEatingFrightenedGhost::GameState->GameState
playerEatingFrightenedGhost gstate | null frightenedGhosts = gstate
                                   | otherwise = gstate { player = newPlayer, ghosts=newGhosts}
  where
    playerPosition = getPlayerPosition $ player gstate
    g = ghosts gstate
    frightenedGhosts = filter (samePosition playerPosition) $ filter (isStateGhost Frightened) g
    notFrightenedGhosts = filter (notSamePosition playerPosition) g
    newState = Retreat
    t = time gstate
    currPlayer = player gstate
    newScore = ((score (player gstate)) + (length frightenedGhosts)*100)
    newPlayer = currPlayer{score = newScore}
    newGhosts = notFrightenedGhosts ++ (setGhostsToState newState frightenedGhosts t)


-- Check if there is a deadly ghost (eg not frightened) on this position
areDeadlyGhostsOnPlayer::Ghosts-> (Int, Int)-> Bool
areDeadlyGhostsOnPlayer ghosts pp = null $ filter (\x ->  x == pp) $ getDeadlyGhostsPosition ghosts

-- Adjust the runningState according to the runningStateGhostOnPlayer function
detectGhostOnPlayer::GameState -> IO GameState
detectGhostOnPlayer gstate = do rsogp <- runningStateGhostOnPlayer gstate (ghosts gstate) (getPlayerPosition $ player gstate)
                                return (gstate {runningState = rsogp})
