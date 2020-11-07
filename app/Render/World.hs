module Render.World where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AssetManager



renderWorld :: GameState -> [Picture]
renderWorld game = parseWorld game $ world game

parseWorld :: GameState -> World -> [Picture]
parseWorld gstate w = map (parseTile gstate) w

parseTile :: GameState -> Tile -> Picture
parseTile gstate t = (translatePictureByTile t (tileToPicture gstate t))


tileToPicture ::GameState -> Tile -> Picture
tileToPicture gstate (NotWalkable wallType (x, y))
  | wallType == VERTICAL = translatePicture (x, y) (getTextureByString gstate "vWall")
  | wallType == HORIZONTAL = translatePicture (x, y) (getTextureByString gstate "hWall")
tileToPicture gstate (Walkable field (x, y))
  | field == Coin = translatePicture (x, y) (getTextureByString gstate "coin")
  | field == Empty = translatePicture (x, y) (getTextureByString gstate "empty")
  | field == Dot = translatePicture (x, y) (getTextureByString gstate "dot")
  | field == DOOR = translatePicture (x, y) (getTextureByString gstate "door")
