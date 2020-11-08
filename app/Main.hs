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

  -- All the game assets used to render the PNGs
  door <- pngByFile "door.png"
  vWall <- pngByFile "vertical.png"
  hWall <- pngByFile "horizontal.png"
  emptySpace <- pngByFile "empty.png"
  coin <- pngByFile "coin.png"
  dot <- pngByFile "dot.png"

  pacmanUp2 <- pngByFile "pacmanUp2.png"
  pacmanDown2 <- pngByFile "pacmanDown2.png"
  pacmanLeft2 <- pngByFile "pacmanLeft2.png"
  pacmanRight2 <- pngByFile "pacmanRight2.png"

  pacmanUp <- pngByFile "pacmanUp.png"
  pacmanDown <- pngByFile "pacmanDown.png"
  pacmanLeft <- pngByFile "pacmanLeft.png"
  pacmanRight <- pngByFile "pacmanRight.png"

  gRed <- pngByFile "red.png"
  gPink <- pngByFile "pink.png"
  gCyan <- pngByFile "cyan.png"
  gOrange <- pngByFile "orange.png"
  gFrightened <- pngByFile "frightened.png"
  gRetreat <- pngByFile "retreat.png"


  gRed2 <- pngByFile "red2.png"
  gPink2 <- pngByFile "pink2.png"
  gCyan2 <- pngByFile "cyan2.png"
  gOrange2 <- pngByFile "orange2.png"
  gFrightened2 <- pngByFile "frightened2.png"

  -- Making a list of all assets
  let assets = Map.fromList [ ("pacmanUp2", pacmanUp2),    ("pacmanDown2", pacmanDown2),    ("pacmanLeft2", pacmanLeft2),    ("pacmanRight2", pacmanRight2),    ("pacmanUp", pacmanUp),    ("pacmanDown", pacmanDown),    ("pacmanLeft", pacmanLeft),    ("pacmanRight", pacmanRight),    ("gRed", gRed),    ("gPink", gPink),    ("gCyan", gCyan),    ("gOrange", gOrange),    ("gFrightened", gFrightened),    ("gRetreat", gRetreat),    ("gRed2", gRed2),    ("gPink2", gPink2), ("gCyan2", gCyan2),    ("gOrange2", gOrange2),    ("gFrightened2", gFrightened2),    ("door" , door ),    ("vWall", vWall),    ("hWall", hWall),    ("empty", emptySpace),    ("coin", coin),    ("dot", dot)  ]


  -- The Gloss IO call to initialise the game.
  playIO window black fps (initialGameState assets) render inputHandler update

-- render -> rendering.hs
-- inputHandler en update -> controller
