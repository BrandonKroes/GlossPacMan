module Render.World where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AssetManager



renderWorld :: GameState -> IO [Picture]
renderWorld game = parseWorld game $ world game

parseWorld :: GameState -> World -> IO [Picture]
parseWorld gstate w = mapM (parseTile gstate) w

parseTile :: GameState -> Tile -> IO Picture
parseTile gstate t = do
                        ttp <- (tileToPicture gstate t)
                        return (translatePictureByTile t ttp)


tileToPicture ::GameState -> Tile -> IO Picture
tileToPicture gstate (NotWalkable wallType (x, y))
  | wallType == VERTICAL = do p <- (walls gstate) !! 1
                              return (translatePicture (x, y) p)
  | wallType == HORIZONTAL = do p <- (walls gstate) !! 2
                                return (translatePicture (x, y) p)
tileToPicture gstate (Walkable field (x, y))
  | field == Coin = do p <- (walls gstate) !! 4
                       return (translatePicture (x, y) p)
  -- TODO: figure out why we had this field | field==Bonus =
  | field == Empty = do p <- (walls gstate) !! 3
                        return (translatePicture (x, y) p)
  | field == Dot = do p <- (walls gstate) !! 5
                      return (translatePicture (x, y) p)
  | field == DOOR = do p <- (walls gstate) !! 0
                       return (translatePicture (x, y) p)
