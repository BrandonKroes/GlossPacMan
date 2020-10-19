{-# LANGUAGE MultiWayIf #-}

module Logic.Ghost where

import Logic.PathFinding
import Logic.Player
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = map (updateGhost gstate) $ ghosts gstate}

updateGhost :: GameState -> Player -> Player
updateGhost gstate (Ghost gPos gColor gState)
  | gState == Frightened = Ghost gPos gColor gState
  | gState == Scatter = Ghost gPos gColor gState
  | gState == Chase = updateGhostByChase (Ghost gPos gColor gState) gstate
updateGhost gstate ghost = ghost


-- Nested guarding isn't a thing in haskell, so abstracting it.
updateGhostByChase::Player -> GameState -> Player
updateGhostByChase (Ghost gPos gColor gState) gstate
    | gColor == RED = getRedGhost (Ghost gPos gColor gState) gstate
    | gColor == PINK = getPinkGhost (Ghost gPos gColor gState) gstate
    | gColor == ORANGE = getOrangeGhost (Ghost gPos gColor gState) gstate
    | gColor == CYAN = getCyanGhost (Ghost gPos gColor gState) gstate
    | otherwise = (Ghost gPos gColor gState)



getOrangeGhost::Player -> GameState -> Player
getOrangeGhost (Ghost gPos gColor gState) gstate = Ghost gPos gColor gState

getCyanGhost::Player->GameState -> Player
getCyanGhost (Ghost gPos gColor gState) gstate = Ghost gPos gColor gState


getRedGhost :: Player -> GameState -> Player
getRedGhost (Ghost gPos gColor gState) gstate = Ghost loc RED gState
  where
    loc = case findPath gstate gPos (position (player gstate)) of
      Just (steps, (x : xs)) -> x
      _ -> gPos

getPinkGhost :: Player -> GameState -> Player
getPinkGhost (Ghost gPos gColor gState) gstate = Ghost loc PINK gState
  where
    newPlayerPos = position (player (updatePosition (updatePosition gstate) ) )  -- update the player position with two steps
    loc = case findPath gstate gPos newPlayerPos of
      Just (steps, (x : xs)) -> x
      _ -> gPos
