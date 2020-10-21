{-# LANGUAGE MultiWayIf #-}

module Logic.Ghost where

import Logic.PathFinding
import Logic.Player
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = map (updateGhost gstate) $ ghosts gstate}

updateGhostDec gstate ghost@(Ghost pos _ _)  = if checkOverlapGhosts new_g gstate then (Ghost pos color gState) else new_g
  where new_g@(Ghost gPos color gState) = updateGhost gstate ghost

updateGhost:: GameState -> Player -> Player
updateGhost gstate (Ghost gPos RED gState)  = getRedGhost (Ghost gPos RED gState) gstate
updateGhost gstate (Ghost gPos PINK gState) = getPinkGhost (Ghost gPos PINK gState) gstate
updateGhost gstate (Ghost gPos ORANGE gState) = getOrangeGhost (Ghost gPos ORANGE gState) gstate
updateGhost gstate (Ghost gPos CYAN gState) = getCyanGhost (Ghost gPos CYAN gState) gstate
updateGhost gstate ghost = ghost

updateGposAstar :: GameState -> (Int, Int) -> (Int, Int) -> (Int, Int)
updateGposAstar gstate start end = case findPath gstate start end of
                                    Just (steps, (x : xs)) -> x
                                    _ -> start

checkOverlapGhosts :: Player -> GameState -> Bool
checkOverlapGhosts (Ghost pos color _) gstate = elem pos ps
  where ps = [p | (Ghost p c _) <- (ghosts gstate), c /= color ]

nextPosScatter::GameState -> (Int, Int) -> (Int, Int) -> ((Int, Int), GhostState)
nextPosScatter gstate start des = let new_pos = updateGposAstar gstate start des in
                                      case new_pos == start of
                                      True -> (start, (Scatter []))
                                      _    -> (new_pos, ToScatterPlace)

getTotalRoute::Player -> [Direction]
getTotalRoute (Ghost _ ORANGE _) = [RIGHT, RIGHT, UP, LEFT, UP, LEFT, DOWN, LEFT, DOWN]
getTotalRoute (Ghost _ CYAN _) = [LEFT, LEFT, UP, RIGHT, UP, RIGHT, DOWN, RIGHT, DOWN] 
getTotalRoute (Ghost _ PINK _) = [DOWN, DOWN, RIGHT, UP, LEFT]
getTotalRoute (Ghost _ RED _) = [DOWN, DOWN, LEFT, UP, RIGHT]

getNewDirPos:: Player -> GameState -> ((Int,Int), [Direction])
getNewDirPos ghost@(Ghost gPos gColor (Scatter route)) gstate = case route of
                                (x:y:xs) -> let new_pos = calculateNextPosition gPos y in
                                              case positionWalkable new_pos gstate of
                                                True -> (new_pos, y:xs)
                                                False -> let new_pos1 = calculateNextPosition gPos x in
                                                  case positionWalkable new_pos1 gstate of
                                                    True -> (new_pos1, x:y:xs)
                                                    _    -> (gPos, (x:y:xs) )
                                [x] -> let new_pos = calculateNextPosition gPos x in
                                              case positionWalkable new_pos gstate of
                                                True -> (new_pos, [x])
                                                _    -> (gPos, getTotalRoute ghost)
                                _   -> (gPos, getTotalRoute ghost)

updateIdleGhost:: Player -> GameState -> Int -> Player
updateIdleGhost ghost@(Ghost gPos gColor Idle ) gstate minCoins =
  if (consumablesTotal gstate) - (consumablesLeft gstate) >= minCoins then (Ghost gPos gColor Chase) else ghost


getOrangeGhost::Player -> GameState -> Player
getOrangeGhost ghost@(Ghost (gx, gy) gColor gState@Chase) gstate = if distance > 8 then getRedGhost ghost gstate else (Ghost (gx, gy) gColor ToScatterPlace) -- follow reds tacic
  where 
    (px, py) = (position (player gstate))
    distance = sqrt (fromIntegral ((px - gx)^2 + (py - gy)^2 )) -- first convert to integral, then sqrt operation

getOrangeGhost (Ghost gPos gColor gState@ToScatterPlace) gstate = Ghost pos gColor state
  where 
    (pos, state) = nextPosScatter gstate gPos (2,29)

getOrangeGhost ghost@(Ghost gPos gColor gState@(Scatter ds)) gstate = Ghost pos gColor (Scatter route)
  where (pos, route) = getNewDirPos ghost gstate

getOrangeGhost ghost@(Ghost gPos gColor Idle) gstate = updateIdleGhost ghost gstate 60

getOrangeGhost g _ = g



getCyanGhost::Player -> GameState -> Player
getCyanGhost (Ghost gPos gColor gState@Chase) gstate = Ghost loc gColor gState
  where 
    (px, py) = position (player (updatePosition (updatePosition gstate) ) ) -- update the player position with two steps
    ((Ghost (rx,ry) _ _ ): gs) = ghosts gstate
    destination = ( 2 * px - rx, 2 * py - ry )
    loc = updateGposAstar gstate gPos destination

getCyanGhost (Ghost gPos gColor gState@ToScatterPlace) gstate = Ghost pos gColor state
  where 
    (pos, state) = nextPosScatter gstate gPos (27,29)

getCyanGhost ghost@(Ghost gPos gColor gState@(Scatter ds)) gstate = Ghost pos gColor (Scatter route)
  where (pos, route) = getNewDirPos ghost gstate

getCyanGhost ghost@(Ghost gPos gColor Idle) gstate = updateIdleGhost ghost gstate 30

getCyanGhost g _ = g



getRedGhost :: Player -> GameState -> Player
getRedGhost (Ghost gPos gColor gState@Chase) gstate = Ghost loc gColor gState
  where
    loc = updateGposAstar  gstate gPos (position (player gstate))

getRedGhost (Ghost gPos gColor gState@ToScatterPlace) gstate = Ghost pos gColor state
  where 
    (pos, state) = nextPosScatter gstate gPos (27,2)

getRedGhost ghost@(Ghost gPos gColor gState@(Scatter ds)) gstate = Ghost pos gColor (Scatter route)
  where (pos, route) = getNewDirPos ghost gstate

getRedGhost ghost@(Ghost gPos gColor Idle) gstate = updateIdleGhost ghost gstate 0

getRedGhost g _ = g



getPinkGhost :: Player -> GameState -> Player
getPinkGhost (Ghost gPos gColor gState@Chase) gstate = Ghost loc gColor gState
  where
    newPlayerPos = position (player (updatePosition (updatePosition (updatePosition (updatePosition gstate) ) ) ))  -- update the player position with four steps
    loc = updateGposAstar gstate gPos newPlayerPos

getPinkGhost (Ghost gPos gColor gState@ToScatterPlace) gstate = Ghost pos gColor state
  where 
    (pos, state) = nextPosScatter gstate gPos (2,2)

getPinkGhost ghost@(Ghost gPos gColor gState@(Scatter ds)) gstate = Ghost pos gColor (Scatter route)
  where (pos, route) = getNewDirPos ghost gstate

getPinkGhost ghost@(Ghost gPos gColor Idle) gstate = updateIdleGhost ghost gstate 5

getPinkGhost g _ = g