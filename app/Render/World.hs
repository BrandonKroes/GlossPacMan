module Render.World where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- * Main call of the module.
-- ** The render World splits
renderWorld :: GameState -> [Picture]
renderWorld game = parseWorld game $ world game

-- ** The world is separated into it's tiles
parseWorld :: GameState -> World -> [Picture]
parseWorld gstate w = map (parseTile gstate) w

-- ** Each tile is converted to a picture and translated to it's position.
parseTile :: GameState -> Tile -> Picture
parseTile gstate t = (translatePictureByTile t (tileToPicture gstate t))


-- * TileToPicture
-- ** There are two 'main' types of tiles. Walkables and Not Walkables
-- ** Each of the mean tiles has their own sub-fields
-- ** Based on the tile the appropriate picture gets fetches from the assets
tileToPicture ::GameState -> Tile -> Picture
tileToPicture gstate (NotWalkable wallType (x, y))
  | wallType == VERTICAL = translatePicture (x, y) (getTextureByString gstate "vWall")
  | wallType == HORIZONTAL = translatePicture (x, y) (getTextureByString gstate "hWall")
tileToPicture gstate (Walkable field (x, y))
  | field == Coin = translatePicture (x, y) (getTextureByString gstate "coin")
  | field == Empty = translatePicture (x, y) (getTextureByString gstate "empty")
  | field == Dot = translatePicture (x, y) (getTextureByString gstate "dot")
  | field == DOOR = translatePicture (x, y) (getTextureByString gstate "door")
