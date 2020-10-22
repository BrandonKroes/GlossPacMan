{-# LANGUAGE MultiWayIf #-}

module Logic.Ghost where
import System.IO.Unsafe
import System.Random
import Logic.PathFinding
import Logic.Player
import Constants
import Model

-- Updating the ghost
updateGhosts :: GameState -> GameState
updateGhosts gstate = gstate {ghosts = newGhostlist}
  where newGhostlist = recCheck gstate [] $ ghosts gstate

recCheck :: GameState -> [Player] -> [Player] -> [Player]     
recCheck gstate new [] = new
recCheck gstate new (x@(Ghost opos ocolor ostate timestamp sequence):xs) = let newG@(Ghost npos ncolor nstate ntimestamp nsequence) = updateGhost gstate x in
                               case npos `elem` [pos | (Ghost pos _ _ _ _) <- xs] || npos `elem` [pos | (Ghost pos _ _ _ _) <- new] of
                                 True -> recCheck gstate ( (Ghost opos ncolor nstate ntimestamp nsequence) : new) xs
                                 _    -> recCheck gstate (newG : new) xs

updateGhost :: GameState -> Player -> Player
updateGhost gstate (Ghost gPos gColor Frightened timestamp sequenc) = updateGhostByFrightened (Ghost gPos gColor Frightened timestamp sequenc) gstate
updateGhost gstate g@(Ghost gPos RED gState _ _ )  = getRedGhost g gstate
updateGhost gstate g@(Ghost gPos PINK gState _ _ ) = getPinkGhost g gstate
updateGhost gstate g@(Ghost gPos ORANGE gState _ _ ) = getOrangeGhost g gstate
updateGhost gstate g@(Ghost gPos CYAN gState _ _ ) = getCyanGhost g gstate
updateGhost gstate ghost = ghost

updateGhostByFrightened::Player->GameState->Player
updateGhostByFrightened (Ghost gPos gColor Frightened timestamp sequenc) gstate
    | (time gstate) >= (timestamp) = setGhostToState Chase (time gstate) (Ghost gPos gColor Frightened timestamp sequenc)
    | otherwise = pickNewRandomPosition (Ghost gPos gColor Frightened timestamp sequenc) gstate
updateGhostByFrightened ghost gstate  = ghost

rng :: Int -> IO Int
rng upper = randomRIO (0,upper-1)

randomElementFromList :: [a] -> IO a
randomElementFromList list = do
  r <- rng (length list)
  return $ list !! r


pickNewRandomPosition::Player->GameState->Player
pickNewRandomPosition (Ghost gPos gColor Frightened timestamp sequenc) gstate = (Ghost newPos gColor Frightened timestamp sequenc)
  where
    newPosOptions = (getWalkableNeighborTilePositions gstate gPos) ++ [gPos]
    amountOfOptions = length newPosOptions
    newPos = unsafePerformIO $ randomElementFromList newPosOptions


updateGposAstar :: GameState -> (Int, Int) -> (Int, Int) -> (Int, Int)
updateGposAstar gstate start end = case findPath gstate start end of
                                    Just (steps, (x : xs)) -> x
                                    _ -> start

nextPosScatter::GameState -> (Int, Int) -> (Int, Int) -> ((Int, Int), GhostState)
nextPosScatter gstate start des = let new_pos = updateGposAstar gstate start des in
                                      case new_pos == start of
                                      True -> (start, (Scatter []))
                                      _    -> (new_pos, ToScatterPlace)

getTotalRoute::Player -> [Direction]
getTotalRoute (Ghost _ ORANGE _ _ _ ) = [RIGHT, RIGHT, UP, LEFT, UP, LEFT, DOWN, LEFT, DOWN]
getTotalRoute (Ghost _ CYAN _ _ _ ) = [LEFT, LEFT, UP, RIGHT, UP, RIGHT, DOWN, RIGHT, DOWN] 
getTotalRoute (Ghost _ PINK _ _ _ ) = [DOWN, DOWN, RIGHT, UP, LEFT]
getTotalRoute (Ghost _ RED _ _ _ ) = [DOWN, DOWN, LEFT, UP, RIGHT]

getNewDirPos:: Player -> GameState -> ((Int,Int), [Direction])
getNewDirPos ghost@(Ghost gPos gColor (Scatter route) _ _) gstate = case route of
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
updateIdleGhost ghost@(Ghost gPos gColor Idle timestamp sequence) gstate minCoins =
  if (consumablesTotal gstate) - (consumablesLeft gstate) >= minCoins then (Ghost gPos gColor Chase timestamp sequence) else ghost


getOrangeGhost::Player -> GameState -> Player
getOrangeGhost ghost@(Ghost (gx, gy) gColor gState@Chase timestamp sequence ) gstate = if distance > 8 then getRedGhost ghost gstate else (Ghost (gx, gy) gColor ToScatterPlace timestamp sequence) -- follow reds tacic
  where 
    (px, py) = (position (player gstate))
    distance = sqrt (fromIntegral ((px - gx)^2 + (py - gy)^2 )) -- first convert to integral, then sqrt operation

getOrangeGhost (Ghost gPos gColor gState@ToScatterPlace timestamp sequence ) gstate = Ghost pos gColor state timestamp sequence
  where 
    (pos, state) = nextPosScatter gstate gPos (2,29)

getOrangeGhost ghost@(Ghost gPos gColor gState@(Scatter ds) timestamp sequence ) gstate = Ghost pos gColor (Scatter route) timestamp sequence
  where (pos, route) = getNewDirPos ghost gstate

getOrangeGhost ghost@(Ghost gPos gColor Idle timestamp sequence) gstate = updateIdleGhost ghost gstate 60

getOrangeGhost g _ = g



getCyanGhost::Player -> GameState -> Player
getCyanGhost (Ghost gPos gColor gState@Chase timestamp sequence) gstate = Ghost loc gColor gState timestamp sequence
  where 
    (px, py) = position (player (updatePosition (updatePosition gstate) ) ) -- update the player position with two steps
    ((Ghost (rx,ry) _ _ _ _): gs) = ghosts gstate
    destination = ( 2 * px - rx, 2 * py - ry )
    loc = updateGposAstar gstate gPos destination

getCyanGhost (Ghost gPos gColor gState@ToScatterPlace timestamp sequence) gstate = Ghost pos gColor state timestamp sequence
  where 
    (pos, state) = nextPosScatter gstate gPos (27,29)

getCyanGhost ghost@(Ghost gPos gColor gState@(Scatter ds) timestamp sequence) gstate = Ghost pos gColor (Scatter route) timestamp sequence
  where (pos, route) = getNewDirPos ghost gstate

getCyanGhost ghost@(Ghost gPos gColor Idle timestamp sequence) gstate = updateIdleGhost ghost gstate 30

getCyanGhost g _ = g



getRedGhost :: Player -> GameState -> Player

getRedGhost (Ghost gPos gColor gState@Chase timestamp sequence) gstate = Ghost loc gColor gState timestamp sequence
  where
    loc = updateGposAstar  gstate gPos (position (player gstate))

getRedGhost (Ghost gPos gColor gState@ToScatterPlace timestamp sequence) gstate = Ghost pos gColor state timestamp sequence
  where 
    (pos, state) = nextPosScatter gstate gPos (27,2)

getRedGhost ghost@(Ghost gPos gColor gState@(Scatter ds) timestamp sequence) gstate = Ghost pos gColor (Scatter route) timestamp sequence
  where (pos, route) = getNewDirPos ghost gstate

getRedGhost ghost@(Ghost gPos gColor Idle timestamp sequence) gstate = updateIdleGhost ghost gstate 0

getRedGhost g _ = g



getPinkGhost :: Player -> GameState -> Player
getPinkGhost (Ghost gPos gColor gState@Chase timestamp sequence) gstate = Ghost loc gColor gState timestamp sequence
  where
    newPlayerPos = position (player (updatePosition (updatePosition (updatePosition (updatePosition gstate) ) ) ))  -- update the player position with four steps
    loc = updateGposAstar gstate gPos newPlayerPos

getPinkGhost (Ghost gPos gColor gState@ToScatterPlace timestamp sequence) gstate = Ghost pos gColor state timestamp sequence
  where 
    (pos, state) = nextPosScatter gstate gPos (2,2)

getPinkGhost ghost@(Ghost gPos gColor gState@(Scatter ds) timestamp sequence) gstate = Ghost pos gColor (Scatter route) timestamp sequence
  where (pos, route) = getNewDirPos ghost gstate

getPinkGhost ghost@(Ghost gPos gColor Idle timestamp sequence) gstate = updateIdleGhost ghost gstate 0

getPinkGhost g _ = g