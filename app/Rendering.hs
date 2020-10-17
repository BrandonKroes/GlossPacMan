module Rendering where

import AssetManager
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model

render :: GameState -> Picture
-- TODO: Make Path dynamic
render g = pictures $ displayOverride g $
                      renderWorld g ++
                      [renderPlayer g] ++
                      renderGhosts g


displayOverride::GameState -> [Picture] -> [Picture]
displayOverride gstate pics | LOST == (runningState gstate) = [(translatePicture (0, 0) (color green (text "U LOSE")))]
                            | WON  == (runningState gstate) = [(translatePicture (0, 0) (color green (text "U DID IT!!"))) ]
                            | otherwise = pics


renderPlayer :: GameState -> Picture
renderPlayer g = renderPlayerType $ player g

getPacManTexture :: Direction -> Picture
getPacManTexture direction | direction==UP=pacmanUp | direction==DOWN=pacmanDown | direction==LEFT=pacmanLeft  | direction==RIGHT= pacmanRight

getGhostTexture::GhostColor->GhostState->Picture
getGhostTexture gcolor state | state== Frightened = gFrightened | gcolor==RED=gRed| gcolor==PINK=gPink| gcolor==CYAN=gCyan|gcolor==ORANGE=gOrange

renderPlayerType :: Player -> Picture
renderPlayerType (PacMan position score direction) = translatePlayerByPosition position $ getPacManTexture direction
renderPlayerType (Ghost position gcolor state ) = translatePlayerByPosition position $ getGhostTexture gcolor state

renderGhosts::GameState -> [Picture]
renderGhosts g = map renderPlayerType $ ghosts g

--- Render world
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
  | field == Coin = translatePicture (x, y) coin
  -- TODO: figure out why we had this field | field==Bonus =
  | field == Empty = translatePicture (x, y) empty
  | field == Dot = translatePicture (x, y) dot
  | field == DOOR = translatePicture (x, y) door




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
