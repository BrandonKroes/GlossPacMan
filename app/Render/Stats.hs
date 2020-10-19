module Render.Stats where

import Model

import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


renderStats::GameState -> [Picture]
renderStats gstate = [(translate (290) (475) (getConsumablesLeft gstate)),
                      (translate (190) (475) (getConsumablesText)),
                      (translate (-90) (475) (getScore gstate)),
                      (translate (-170) (475) (getScoreText)),
                      (translate (-320) (475) (getTime gstate)),
                      (translate (-390) (475) (getTimeLabel))
                      ]

getTimeSeconds::Float->String
getTimeSeconds secs = show $ ((round secs) `mod` 60::Int)

getTimeMinutes::Float->String
getTimeMinutes minutes = show $ ((round minutes) `quot` 60::Int)

getTime::GameState->Picture
getTime gstate = scale 0.15 0.15 $ color white (text (getTimeMinutes (time gstate) ++ ":" ++ getTimeSeconds (time gstate) ))

getTimeLabel::Picture
getTimeLabel = scale 0.15 0.15 $ color white (text "Time: ")



getConsumablesLeft::GameState->Picture
getConsumablesLeft gstate = scale 0.15 0.15 $ color white (text (show $ consumablesLeft gstate ))

getConsumablesText::Picture
getConsumablesText = scale 0.15 0.15 $ color white (text "Items left: ")

getScoreText::Picture
getScoreText = scale 0.15 0.15 $ color white (text "Score: ")

getScore::GameState->Picture
getScore gstate = scale 0.15 0.15 $ color white (text (show $ ((consumablesTotal gstate) - (consumablesLeft gstate)) * 100))
