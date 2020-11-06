module Render.Player where

import AssetManager

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


renderPlayers::GameState -> [Picture]
renderPlayers gstate = (renderPlayer gstate) ++ (renderGhosts gstate)

renderPlayer :: GameState -> [Picture]
renderPlayer g = [renderPlayerType  g $ player g]


getPacManTexture::Direction -> Int -> Picture
getPacManTexture direction 1 = getPacManIntervalTexture direction
getPacManTexture direction 2 = getPacManAlternatingIntervalTexture direction


getPacManIntervalTexture::Direction -> Picture
getPacManIntervalTexture direction | direction == UP = pacmanUp
  | direction == DOWN = pacmanDown
  | direction == LEFT = pacmanLeft
  | direction == RIGHT = pacmanRight

getPacManAlternatingIntervalTexture::Direction -> Picture
getPacManAlternatingIntervalTexture direction | direction == UP = pacmanUp2
  | direction == DOWN = pacmanDown2
  | direction == LEFT = pacmanLeft2
  | direction == RIGHT = pacmanRight2

getGhostTexture :: GhostColor -> GhostState -> Int -> Picture
getGhostTexture gColor Frightened 1 = gFrightened
getGhostTexture gColor Frightened 2 = gFrightened2
getGhostTexture gColor Retreat _ = gRetreat
getGhostTexture gColor _ 1 = getGhostIntervalTexture gColor
getGhostTexture gColor _ 2 = getGhostAlternatingTexture gColor

getGhostIntervalTexture::GhostColor -> Picture
getGhostIntervalTexture gColor | gColor == RED = gRed
  | gColor == PINK = gPink
  | gColor == CYAN = gCyan
  | gColor == ORANGE = gOrange

getGhostAlternatingTexture::GhostColor -> Picture
getGhostAlternatingTexture gColor
  | gColor == RED = gRed2
  | gColor == PINK = gPink2
  | gColor == CYAN = gCyan2
  | gColor == ORANGE = gOrange2

renderPlayerType :: GameState -> Player -> Picture
renderPlayerType gstate (PacMan position score (direction,_))  = translatePlayerByPosition position $ getPacManTexture direction (animationInterval gstate)
renderPlayerType gstate (Ghost position gColor state timestamp sequenc direction)  = translatePlayerByPosition position $ getGhostTexture gColor state $ animationInterval gstate

renderGhosts :: GameState -> [Picture]
renderGhosts gstate = map (renderPlayerType gstate)  (ghosts gstate)
