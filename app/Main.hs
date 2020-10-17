module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Model
import Controller
import Rendering

window = InWindow "PacMan" (screenWidth, screenHeight) (100, 100)

initialGameState::GameState
initialGameState = GameState {
                    runningState=RUNNING,
                    player= PacMan  (14, 23) 0 UP,
                    ghosts= [  --Ghost (13, 14) RED Chase,
                               Ghost (14, 14) ORANGE Chase,
                               --Ghost (15, 14) PINK Chase,
                               Ghost (16, 14) CYAN Chase
                              ],
                    world=getDefaultWorld,
                    pause=False,
                    dotsLeft= countAmountOfDots getDefaultWorld
                  }


main :: IO ()
main = play window black 10 initialGameState render inputHandler update
