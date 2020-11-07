module Render.Override where

import Model
import Render.Util
import Render.Stats

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Render override can decide to throw away all the pics.
-- Purposefully not using pattern matching, because it would cause 5 gigantic lines to adjust each time the code base changes
renderOverride::GameState -> [Picture] -> [Picture]
renderOverride gstate frames | LOST == (runningState gstate) = getLostText gstate
                             | WON  == (runningState gstate) = getWonText
                             | START == (runningState gstate) = getStartText
                             | PAUSE == (runningState gstate) = getPauseText
                             | otherwise = frames

getLostText::GameState->[Picture]
getLostText gstate = [(translatePicture(-100, 0  ) (color green (text "YOU LOSE"))),
              (translatePicture (200, 100) (getScoreText)),
              (translatePicture (300, 100) (getScore gstate)),
              (translatePicture (-100, 350) (color red   (text "PRESS ANY"))),
              (translatePicture (-100, 470) (color red   (text " KEY TO"))),
              (translatePicture (-100, 590) (color red   (text "TRY AGAIN")))]


getPauseText::[Picture]
getPauseText = [(translatePicture (-100, 0) (color green (text "PAUSED"))),
              (translatePicture (-100, 350) (color blue (text "PRESS 'P'"))),
              (translatePicture (-100, 470) (color blue (text " TO"))),
              (translatePicture (-100, 590) (color blue (text "CONTINUE")))]

getWonText::[Picture]
getWonText = [(translatePicture (-100, 0) (color green (text "YOU WIN"))),
              (translatePicture (-100, 350) (color blue (text "PRESS ANY"))),
              (translatePicture (-100, 470) (color blue (text " KEY TO"))),
              (translatePicture (-100, 590) (color blue (text "GO AGAIN")))]

getStartText::[Picture]
getStartText = [(translatePicture (-100, 0) (color green (text "WELCOME!"))),
              (translatePicture (-100, 350) (color yellow (text "PRESS ANY"))),
              (translatePicture (-260, 470) (color yellow (text "KEY TO START")))
              ]
