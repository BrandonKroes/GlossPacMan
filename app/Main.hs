module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game

import Model
import Controller
import Rendering
import Constants

window = InWindow "PacMan" (screenWidth, screenHeight) (100, 100)



main :: IO ()
main = playIO window black fps initialGameState render inputHandler update

-- render -> rendering.hs
-- inputHandler en update -> controller
