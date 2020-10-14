module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Model
import Logic
import Rendering

window = InWindow "PacMan" (screenWidth, screenHeight) (100, 100)

isDot::Tile->Bool
isDot (Walkable field _) | field==Dot=True | otherwise=False
isDot _ = False

countAmountOfDots::World->Int
countAmountOfDots w = length $ filter (isDot) w

initialGameState::GameState
initialGameState = GameState {
                    player= PacMan (10,10) 0 UP,
                    ghosts= [  Ghost (1, 1) RED Chase,
                               Ghost (2, 2) ORANGE Chase,
                               Ghost (3, 3) PINK Chase,
                               Ghost (4, 4) CYAN Chase
                              ],
                    world=getDefaultWorld,
                    pause=False,
                    dotsLeft=countAmountOfDots getDefaultWorld
                  }


main :: IO ()
main = play window black 60 initialGameState render transformGameState update
