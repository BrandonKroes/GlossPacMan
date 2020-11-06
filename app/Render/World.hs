module Render.World where

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import AssetManager



renderWorld :: GameState -> IO [Picture]
renderWorld game = parseWorld $ world game

parseWorld :: World -> IO [Picture]
parseWorld w = mapM parseTile w

parseTile :: Tile -> IO Picture
parseTile t = do ttp <- tileToPicture t
                 return (translatePictureByTile t ttp)

tileToPicture :: Tile -> IO Picture
tileToPicture (NotWalkable wallType (x, y))
  | wallType == VERTICAL = do p <- wVertical
                              return (translatePicture (x, y) p)
  | wallType == HORIZONTAL = do p <- wHorizontal
                                return (translatePicture (x, y) p)
tileToPicture (Walkable field (x, y))
  | field == Coin = do p <- imgCoin
                       return (translatePicture (x, y) p)
  -- TODO: figure out why we had this field | field==Bonus =
  | field == Empty = do p <- imgEmpty
                        return (translatePicture (x, y) p)
  | field == Dot = do p <- imgDot 
                      return (translatePicture (x, y) p)
  | field == DOOR = do p <- imgDoor
                       return (translatePicture (x, y) p)
