module Render.Util where

import Data.Maybe
import qualified Data.Map.Strict as Map

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import Constants


-- Translation section
-- Most computer graphics orient their POV from the top-left to bottom right.
-- For some kind of magical reason Gloss DOESN'T.
-- In order to use calculations based on this principle, we will convert it.
translatePicture :: (Int, Int) -> Picture -> Picture
translatePicture (x, y) pic = translate (originHeight + fromIntegral (x)) (originWidth - fromIntegral (y)) pic

translatePlayerByPosition::(Int, Int) -> Picture -> Picture
translatePlayerByPosition (x, y) pic = translate (fromIntegral (x*33) - (originWidth*2)) (-fromIntegral (y*32) - originHeight*2) pic

translatePictureByBoundary :: (Int, Int) -> Picture -> Picture
translatePictureByBoundary (x, y) pic = translatePicture (x * 31, y * 31) pic

-- ** Translating a tile requires the tile index and the picture. In this function we pattern match on these values.
translatePictureByTile :: Tile -> Picture -> Picture
translatePictureByTile (NotWalkable _ p) picture = translatePictureByBoundary p picture
translatePictureByTile (Walkable f p) picture = translatePictureByBoundary p picture

-- * Getting a texture, requires the key (the name) and the game state.
-- ** The function will return a picture insance.
-- ** Any error arrising from getting the image should crash the game.
-- ** The game doesn't present itself in a playable state if there are textures missing.
getTextureByString::GameState->String->Picture
getTextureByString gstate textureString = fromJust $ Map.lookup textureString (assets gstate)
