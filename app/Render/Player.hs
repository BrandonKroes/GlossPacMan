module Render.Player where

import Model
import Render.Util
import Graphics.Gloss

-- Get the pictures of ghosts and PacMan
renderPlayers::GameState -> [Picture]
renderPlayers gstate = (renderPlayer gstate) ++ (renderGhosts gstate)

-- Get pictures depending on the player kind (PacMan or Ghost)
renderPlayer :: GameState -> [Picture]
renderPlayer g = [renderPlayerType  g $ player g]

-- Get the texture of PacMan
getPacManTexture::GameState -> Direction -> Int -> Picture
getPacManTexture gstate direction 1 = getPacManIntervalTexture gstate direction
getPacManTexture gstate direction 2 = getPacManAlternatingIntervalTexture gstate direction

-- Get the texture of PacMan according to its direction
getPacManIntervalTexture::GameState -> Direction -> Picture
getPacManIntervalTexture gstate direction | direction == UP = (getTextureByString gstate "pacmanUp")
  | direction == DOWN = (getTextureByString gstate "pacmanDown")
  | direction == LEFT = (getTextureByString gstate "pacmanLeft")
  | direction == RIGHT = (getTextureByString gstate "pacmanRight")

-- Get the texture of the alternating PacMan according to its direction
getPacManAlternatingIntervalTexture::GameState -> Direction -> Picture
getPacManAlternatingIntervalTexture gstate direction | direction == UP = (getTextureByString gstate "pacmanUp2")
  | direction == DOWN = (getTextureByString gstate "pacmanDown2")
  | direction == LEFT = (getTextureByString gstate "pacmanLeft2")
  | direction == RIGHT = (getTextureByString gstate "pacmanRight2")

-- Get the texture of Ghost, depending on the state and color
getGhostTexture :: GameState -> GhostColor -> GhostState -> Int -> Picture
getGhostTexture gstate gColor Frightened 1 = (getTextureByString gstate "gFrightened")
getGhostTexture gstate gColor Frightened 2 = (getTextureByString gstate "gFrightened2")
getGhostTexture gstate gColor Retreat _ = (getTextureByString gstate "gRetreat")
getGhostTexture gstate gColor _ 1 = getGhostIntervalTexture gstate gColor
getGhostTexture gstate gColor _ 2 = getGhostAlternatingTexture gstate  gColor

-- Get the texture of Ghost according to its color
getGhostIntervalTexture::GameState -> GhostColor -> Picture
getGhostIntervalTexture gstate gColor | gColor == RED = (getTextureByString gstate "gRed")
  | gColor == PINK = (getTextureByString gstate "gPink")
  | gColor == CYAN = (getTextureByString gstate "gCyan")
  | gColor == ORANGE = (getTextureByString gstate "gOrange")

-- Get the texture of the alternating Ghost according to its color
getGhostAlternatingTexture::GameState->GhostColor -> Picture
getGhostAlternatingTexture gstate gColor
  | gColor == RED = (getTextureByString gstate "gRed2")
  | gColor == PINK = (getTextureByString gstate "gPink2")
  | gColor == CYAN = (getTextureByString gstate "gCyan2")
  | gColor == ORANGE = (getTextureByString gstate "gOrange2")

-- Get the picture according to kind of the player
renderPlayerType :: GameState -> Player -> Picture
renderPlayerType gstate (PacMan position score (direction,_))  = translatePlayerByPosition position $  getPacManTexture gstate direction (animationInterval gstate)
renderPlayerType gstate (Ghost position gColor state timestamp sequenc direction)  = translatePlayerByPosition position $ getGhostTexture gstate gColor state $ animationInterval gstate

-- Get all ghost textures
renderGhosts :: GameState -> [Picture]
renderGhosts gstate = map (renderPlayerType gstate)  (ghosts gstate)
