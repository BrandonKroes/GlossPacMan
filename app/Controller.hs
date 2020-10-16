module Controller where

import Data.Foldable (asum)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Logic.FrameTime
import Logic.Ghost
import Logic.Player
import Logic.World
import Model

-- The execution order of the gamestate is import for change detection
update :: Float -> GameState -> GameState
update currentFT gstate
  -- check if the game is paused
  -- TODO: Find out if YODA conditions are considered good practice in Haskell.
  | True == pause gstate = gstate
  | otherwise =
    updateWorld
      $ updateGhosts
      $ updatePlayer
      $ updateFrameTime gstate currentFT

inputHandler :: Event -> GameState -> GameState
inputHandler event gstate
  | False == (pause gstate) = case event of
    EventKey (Char 'w') Down _ _ -> setPlayerDirection UP gstate
    EventKey (Char 's') Down _ _ -> setPlayerDirection DOWN gstate
    EventKey (Char 'a') Down _ _ -> setPlayerDirection LEFT gstate
    EventKey (Char 'd') Down _ _ -> setPlayerDirection RIGHT gstate
    EventKey (Char 'p') Down _ _ -> gstate {pause = not $ pause gstate}
    _ -> gstate
  | True == (pause gstate) = case event of
    EventKey (Char 'p') Down _ _ -> gstate {pause = not $ pause gstate}
    _ -> gstate
