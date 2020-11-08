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

-- The Update function is called every frame and calls all the update calls from the logic module.
update :: Float -> GameState -> IO GameState
update currentFT gstate
  -- check if the game is paused
  | runningState gstate /= RUNNING = return gstate
  | otherwise = do c <- conditions $ updatePlayer $ updateFrameTime gstate currentFT
                   uG <- updateGhosts c
                   c2 <- conditions uG
                   return (updateWorld $ c2)

-- * This function is used to handle user input when the player isn't playing the game (So on a start or stop screen)
inputHandler :: Event -> GameState -> IO GameState
inputHandler event gstate
  | (runningState gstate) /= RUNNING = case event of
                                        EventKey (SpecialKey _) Down _ _ -> return (switchToRunningState gstate)
                                        EventKey (Char _) Down _ _ -> return (switchToRunningState gstate)
                                        _ -> return gstate
  | otherwise = return (runningInputHandler event gstate)

-- * This function is applied when the game is playing.
runningInputHandler::Event -> GameState -> GameState
runningInputHandler event gstate
    | not (isPaused gstate) = case event of
                           EventKey (Char 'w') Down _ _ -> setPlayerDirection UP gstate
                           EventKey (Char 's') Down _ _ -> setPlayerDirection DOWN gstate
                           EventKey (Char 'a') Down _ _ -> setPlayerDirection LEFT gstate
                           EventKey (Char 'd') Down _ _ -> setPlayerDirection RIGHT gstate
                           EventKey (Char 'p') Down _ _ -> flipPause gstate
                           _ -> gstate
   | otherwise = case event of
                 EventKey (Char 'p') Down _ _ -> flipPause gstate
                 _ -> gstate
