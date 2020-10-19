module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Model
import Controller
import Rendering

window = InWindow "PacMan" (screenWidth, screenHeight) (100, 100)



main :: IO ()
main = play window black 10 initialGameState render inputHandler update
