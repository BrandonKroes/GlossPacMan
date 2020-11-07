module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Map.Strict as Map
import Model
import Controller
import Rendering
import Constants
import AssetManager

window = InWindow "PacMan" (screenWidth, screenHeight) (100, 100)



main :: IO ()
main = do

  door <- pngByFile "door.png"
  vWall <- pngByFile "vertical.png"
  hWall <- pngByFile "horizontal.png"
  emptySpace <- pngByFile "empty.png"
  coin <- pngByFile "coin.png"
  dot <- pngByFile "dot.png"
  let walls = Map.fromList [("door", door), ("vWall", vWall), ("hWall", hWall), ("empty", emptySpace), ("coin", coin), ("dot", dot)]

  playIO window black fps (initialGameState walls) render inputHandler update

-- render -> rendering.hs
-- inputHandler en update -> controller
