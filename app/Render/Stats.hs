module Render.Stats where

import Model

import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- * Render call for the statistics on the top of the menu
renderStats::GameState -> [Picture]
renderStats gstate = [(translate (290) (475) (getConsumablesLeft gstate)),
                      (translate (190) (475) (getConsumablesText)),
                      (translate (-90) (475) (getScore gstate)),
                      (translate (-170) (475) (getScoreText)),
                      (translate (-320) (475) (getTime gstate)),
                      (translate (-390) (475) (getTimeLabel))
                      ]

-- * Parsing the total game time to seconds for the time (minutes:seconds) format
getTimeSeconds::Float->String
getTimeSeconds sec | len > 1 = secs | otherwise = secsappend
      where
        secs = getSec sec
        len = length secs
        secsappend = "0" ++ secs

getSec::Float -> String
getSec sec = show $ ((round sec) `mod` 60::Int)


-- * Parsing the total game time to minutes for the time (minutes:seconds) format
getTimeMinutes::Float->String
getTimeMinutes minutes = show $ ((round minutes) `quot` 60::Int)


-- * Making the time display picture
getTime::GameState->Picture
getTime gstate = scale 0.15 0.15 $ color white (text (getTimeMinutes (time gstate) ++ ":" ++ getTimeSeconds (time gstate) ))

-- * Label to prepend the time picture with
getTimeLabel::Picture
getTimeLabel = scale 0.15 0.15 $ color white (text "Time: ")


-- * Label to display that the consumables left is displayed right
getConsumablesText::Picture
getConsumablesText = scale 0.15 0.15 $ color white (text "Items left: ")

-- * Label to display all the items left in the map
getConsumablesLeft::GameState->Picture
getConsumablesLeft gstate = scale 0.15 0.15 $ color white (text (show $ consumablesLeft gstate ))



-- * Label to display that the score text is displayed right
getScoreText::Picture
getScoreText = scale 0.15 0.15 $ color white (text "Score: ")


-- * Label to display the player score
getScore::GameState->Picture
getScore gstate = scale 0.15 0.15 $ color white (text (show currScore ))
  where currScore = (((consumablesTotal gstate) - (consumablesLeft gstate)) * 100) + (score (player gstate))
