module Controller where

import Data.Foldable (asum)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Logic.FrameTime
import Logic.Ghost
import Logic.Player
import Logic.World
import Logic.Conditions
import Model

-- The execution order of the gamestate is import for change detection
update :: Float -> GameState -> IO GameState
update currentFT gstate
  -- check if the game is paused
  -- TODO: Find out if YODA conditions are considered good practice in Haskell.
  | pause gstate = return gstate
  | runningState gstate /= RUNNING = return gstate
  | otherwise = do uG <- updateGhosts $ updatePlayer $ updateFrameTime gstate currentFT
                   conditions $ updateWorld $ uG

inputHandler :: Event -> GameState -> IO GameState
inputHandler event gstate
  | (runningState gstate) /= RUNNING = case event of
                                        EventKey (SpecialKey _) Down _ _ -> return runningGameState
                                        EventKey (Char _) Down _ _ -> return runningGameState
                                        _ -> return gstate
  | otherwise = return (runningInputHandler event gstate)

runningInputHandler::Event -> GameState -> GameState
runningInputHandler event gstate
    | not (pause gstate) = case event of
                           EventKey (Char 'w') Down _ _ -> setPlayerDirection UP gstate
                           EventKey (Char 's') Down _ _ -> setPlayerDirection DOWN gstate
                           EventKey (Char 'a') Down _ _ -> setPlayerDirection LEFT gstate
                           EventKey (Char 'd') Down _ _ -> setPlayerDirection RIGHT gstate
                           EventKey (Char 'p') Down _ _ -> gstate {pause = not $ pause gstate}
                           _ -> gstate
   | otherwise = case event of
                 EventKey (Char 'p') Down _ _ -> gstate {pause = not $ pause gstate}
                 _ -> gstate


-- The quit buttons stores the current game state
