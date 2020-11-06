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
update :: Float -> GameState -> GameState
update currentFT gstate
  | runningState gstate /= RUNNING = gstate
  | otherwise = conditions
      $ updateWorld
      $ updateGhosts
      $ updatePlayer
      $ updateFrameTime gstate currentFT

inputHandler :: Event -> GameState -> GameState
inputHandler event gstate
  | not (isPlayState gstate)= case event of
                                        EventKey (SpecialKey _) Down _ _ -> runningGameState
                                        EventKey (Char _) Down _ _ -> runningGameState
                                        _ -> gstate
  | otherwise = runningInputHandler event gstate

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


-- The quit buttons stores the current game state
