{-# LANGUAGE MultiWayIf #-}

module Logic.Ghost where

import Logic.PathFinding
import Logic.Player
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = map (updateGhost gstate) $ ghosts gstate}

updateGhost :: GameState -> Player -> Player
updateGhost gstate (Ghost gPos gColor Frightened) = Ghost gPos gColor Frightened
updateGhost gstate (Ghost gPos gColor Scatter)  = Ghost gPos gColor Scatter
updateGhost gstate (Ghost gPos gColor Chase) = updateGhostByChase (Ghost gPos gColor Chase) gstate
updateGhost gstate ghost = ghost


-- Nested guarding isn't a thing in haskell, so abstracting it.
updateGhostByChase::Player -> GameState -> Player
updateGhostByChase (Ghost gPos RED gState) gstate = getRedGhost (Ghost gPos RED gState) gstate
updateGhostByChase (Ghost gPos PINK gState) gstate = getPinkGhost (Ghost gPos PINK gState) gstate
updateGhostByChase (Ghost gPos ORANGE gState) gstate = getOrangeGhost (Ghost gPos ORANGE gState) gstate
updateGhostByChase (Ghost gPos CYAN gState) gstate = getCyanGhost (Ghost gPos CYAN gState) gstate
updateGhostByChase ghost gstate = ghost



getOrangeGhost::Player -> GameState -> Player
getOrangeGhost og@(Ghost (gx, gy) gColor gState) gstate = if distance > 8 then getRedGhost og gstate else (Ghost (gx, gy) gColor Scatter)
  where 
    (px, py) = (position (player gstate))
    distance = sqrt (fromIntegral ((px - gx)^2 + (py - gy)^2 ))
    

getCyanGhost::Player -> GameState -> Player
getCyanGhost (Ghost gPos gColor gState) gstate = Ghost loc gColor gState
  where 
    (px, py) = position (player (updatePosition (updatePosition gstate) ) ) -- update the player position with two steps
    ((Ghost (rx,ry) _ _ ): gs) = ghosts gstate
    destination = ( 2 * px - rx, 2 * py - ry )
    loc = case findPath gstate gPos destination of
      Just (steps, (x : xs)) -> x
      _ -> gPos

getRedGhost :: Player -> GameState -> Player
getRedGhost (Ghost gPos gColor gState) gstate = Ghost loc gColor gState
  where
    loc = case findPath gstate gPos (position (player gstate)) of
      Just (steps, (x : xs)) -> x
      _ -> gPos

getPinkGhost :: Player -> GameState -> Player
getPinkGhost (Ghost gPos gColor gState) gstate = Ghost loc gColor gState
  where
    newPlayerPos = position (player (updatePosition (updatePosition (updatePosition (updatePosition gstate) ) ) ))  -- update the player position with four steps
    loc = case findPath gstate gPos newPlayerPos of
      Just (steps, (x : xs)) -> x
      _ -> gPos
