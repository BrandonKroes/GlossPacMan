module Render.Player where

import AssetManager

import Model
import Render.Util
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


renderPlayers::GameState -> IO [Picture]
renderPlayers gstate = do 
                      rp <- renderPlayer gstate
                      rg <- renderGhosts gstate
                      return (rp ++ rg)

renderPlayer :: GameState -> IO [Picture]
renderPlayer g = do 
                   p <- renderPlayerType  g $ player g
                   return [p]


getPacManTexture::Direction -> Int -> IO Picture
getPacManTexture direction 1 = getPacManIntervalTexture direction
getPacManTexture direction 2 = getPacManAlternatingIntervalTexture direction


getPacManIntervalTexture::Direction -> IO Picture
getPacManIntervalTexture direction | direction == UP = pacmanUp
  | direction == DOWN = pacmanDown -- assetmangaer.hs
  | direction == LEFT = pacmanLeft
  | direction == RIGHT = pacmanRight

getPacManAlternatingIntervalTexture::Direction -> IO Picture
getPacManAlternatingIntervalTexture direction | direction == UP = pacmanUp2
  | direction == DOWN = pacmanDown2
  | direction == LEFT = pacmanLeft2
  | direction == RIGHT = pacmanRight2

getGhostTexture :: GhostColor -> GhostState -> Int -> IO Picture
getGhostTexture gColor Frightened 1 = gFrightened
getGhostTexture gColor Frightened 2 = gFrightened2
getGhostTexture gColor Retreat _ = gRetreat
getGhostTexture gColor _ 1 = getGhostIntervalTexture gColor
getGhostTexture gColor _ 2 = getGhostAlternatingTexture gColor

getGhostIntervalTexture::GhostColor -> IO Picture
getGhostIntervalTexture gColor | gColor == RED = gRed
  | gColor == PINK = gPink
  | gColor == CYAN = gCyan
  | gColor == ORANGE = gOrange

getGhostAlternatingTexture::GhostColor -> IO Picture
getGhostAlternatingTexture gColor
  | gColor == RED = gRed2
  | gColor == PINK = gPink2
  | gColor == CYAN = gCyan2
  | gColor == ORANGE = gOrange2

renderPlayerType :: GameState -> Player -> IO Picture
renderPlayerType gstate (PacMan position score (direction,_))  = do 
                                                                    t <- getPacManTexture direction (animationInterval gstate)
                                                                    return (translatePlayerByPosition position t)

renderPlayerType gstate (Ghost position gColor state timestamp sequenc direction)  = do 
                                                                                     t <- getGhostTexture gColor state $ animationInterval gstate
                                                                                     return (translatePlayerByPosition position t)

renderGhosts :: GameState -> IO [Picture]
renderGhosts gstate = mapM (renderPlayerType gstate)  (ghosts gstate)
