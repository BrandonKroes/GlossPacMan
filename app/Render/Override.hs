module Render.Override where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game



-- Render override can decide to throw away all the pics.
-- Future optimisation would prevent generation of pictures, but currently it will only prevent rendering.
renderOverride::GameState -> [Picture] -> [Picture]
renderOverride gstate frames | LOST == (runningState gstate) = getLostText
                             | WON  == (runningState gstate) = getWonText
                             | START == (runningState gstate) = getStartText
                             | otherwise = frames


getLostText::[Picture]
getLostText = [(translatePicture (-100, 0) (color green (text "YOU LOSE"))),
              (translatePicture (-100, 350) (color red (text "PRESS ANY"))),
              (translatePicture (-100, 470) (color red (text " KEY TO"))),
              (translatePicture (-100, 590) (color red (text "TRY AGAIN")))]

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
