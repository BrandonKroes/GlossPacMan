module Render.Player where

import AssetManager

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


renderPlayers::GameState -> [Picture]
renderPlayers gstate = (renderPlayer gstate) ++ (renderGhosts gstate)

renderPlayer :: GameState -> [Picture]
renderPlayer g = [renderPlayerType $ player g]

getPacManTexture :: Direction -> Picture
getPacManTexture direction | direction == UP = pacmanUp
  | direction == DOWN = pacmanDown
  | direction == LEFT = pacmanLeft
  | direction == RIGHT = pacmanRight

getGhostTexture :: GhostColor -> GhostState -> Picture
getGhostTexture gColor state
  | state == Frightened = gFrightened
  | gColor == RED = gRed
  | gColor == PINK = gPink
  | gColor == CYAN = gCyan
  | gColor == ORANGE = gOrange

renderPlayerType :: Player -> Picture
renderPlayerType (PacMan position score direction) = translatePlayerByPosition position $ getPacManTexture direction
renderPlayerType (Ghost position gColor state) = translatePlayerByPosition position $ getGhostTexture gColor state

renderGhosts :: GameState -> [Picture]
renderGhosts g = map renderPlayerType $ ghosts g
