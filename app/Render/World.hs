module Render.World where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AssetManager



renderWorld :: GameState -> [Picture]
renderWorld game = parseWorld $ world game

parseWorld :: World -> [Picture]
parseWorld w = map parseTile w

parseTile :: Tile -> Picture
parseTile t = translatePictureByTile t $ tileToPicture t

tileToPicture :: Tile -> Picture
tileToPicture (NotWalkable wallType (x, y))
  | wallType == VERTICAL = translatePicture (x, y) wVertical
  | wallType == HORIZONTAL = translatePicture (x, y) wHorizontal
tileToPicture (Walkable field (x, y))
  | field == Coin = translatePicture (x, y) imgCoin
  -- TODO: figure out why we had this field | field==Bonus =
  | field == Empty = translatePicture (x, y) imgEmpty
  | field == Dot = translatePicture (x, y) imgDot
  | field == DOOR = translatePicture (x, y) imgDoor
