module Render.Util where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Translation section
-- Most computer graphics orient their POV from the top-left to bottom right.
-- For some kind of magical reason gloss DOESN'T.
-- In order to use calculations based on this principle, we will convert it.
translatePicture :: (Int, Int) -> Picture -> Picture
translatePicture (x, y) pic = translate (originHeight + fromIntegral (x)) (originWidth - fromIntegral (y)) pic

translatePlayerByPosition::(Int, Int) -> Picture -> Picture
translatePlayerByPosition (x, y) pic = translate (fromIntegral (x*33) - (originWidth*2)) (-fromIntegral (y*32) - originHeight*2) pic

translatePictureByBoundary :: (Int, Int) -> Picture -> Picture
translatePictureByBoundary (x, y) pic = translatePicture (x * 31, y * 31) pic

translatePictureByTile :: Tile -> Picture -> Picture
translatePictureByTile (NotWalkable _ p) picture = translatePictureByBoundary p picture
translatePictureByTile (Walkable f p) picture = translatePictureByBoundary p picture
