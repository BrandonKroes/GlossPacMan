module Render.Player where

import Model
import Render.Util
import Graphics.Gloss


renderPlayers::GameState -> [Picture]
renderPlayers gstate = (renderPlayer gstate) ++ (renderGhosts gstate)

renderPlayer :: GameState -> [Picture]
renderPlayer g = [renderPlayerType  g $ player g]


getPacManTexture::GameState -> Direction -> Int -> Picture
getPacManTexture gstate direction 1 = getPacManIntervalTexture gstate direction
getPacManTexture gstate direction 2 = getPacManAlternatingIntervalTexture gstate direction


getPacManIntervalTexture::GameState -> Direction -> Picture
getPacManIntervalTexture gstate direction | direction == UP = (getTextureByString gstate "pacmanUp")
  | direction == DOWN = (getTextureByString gstate "pacmanDown")
  | direction == LEFT = (getTextureByString gstate "pacmanLeft")
  | direction == RIGHT = (getTextureByString gstate "pacmanRight")

getPacManAlternatingIntervalTexture::GameState -> Direction -> Picture
getPacManAlternatingIntervalTexture gstate direction | direction == UP = (getTextureByString gstate "pacmanUp2")
  | direction == DOWN = (getTextureByString gstate "pacmanDown2")
  | direction == LEFT = (getTextureByString gstate "pacmanLeft2")
  | direction == RIGHT = (getTextureByString gstate "pacmanRight2")

getGhostTexture :: GameState -> GhostColor -> GhostState -> Int -> Picture
getGhostTexture gstate gColor Frightened 1 = (getTextureByString gstate "gFrightened")
getGhostTexture gstate gColor Frightened 2 = (getTextureByString gstate "gFrightened2")
getGhostTexture gstate gColor Retreat _ = (getTextureByString gstate "gRetreat")
getGhostTexture gstate gColor _ 1 = getGhostIntervalTexture gstate gColor
getGhostTexture gstate gColor _ 2 = getGhostAlternatingTexture gstate  gColor

getGhostIntervalTexture::GameState -> GhostColor -> Picture
getGhostIntervalTexture gstate gColor | gColor == RED = (getTextureByString gstate "gRed")
  | gColor == PINK = (getTextureByString gstate "gPink")
  | gColor == CYAN = (getTextureByString gstate "gCyan")
  | gColor == ORANGE = (getTextureByString gstate "gOrange")

getGhostAlternatingTexture::GameState->GhostColor -> Picture
getGhostAlternatingTexture gstate gColor
  | gColor == RED = (getTextureByString gstate "gRed2")
  | gColor == PINK = (getTextureByString gstate "gPink2")
  | gColor == CYAN = (getTextureByString gstate "gCyan2")
  | gColor == ORANGE = (getTextureByString gstate "gOrange2")

renderPlayerType :: GameState -> Player -> Picture
renderPlayerType gstate (PacMan position score (direction,_))  = translatePlayerByPosition position $  getPacManTexture gstate direction (animationInterval gstate)
renderPlayerType gstate (Ghost position gColor state timestamp sequenc direction)  = translatePlayerByPosition position $ getGhostTexture gstate gColor state $ animationInterval gstate

renderGhosts :: GameState -> [Picture]
renderGhosts gstate = map (renderPlayerType gstate)  (ghosts gstate)
