{-# LANGUAGE MultiWayIf #-}

module Logic.Ghost where
import System.IO.Unsafe
import System.Random
import Logic.PathFinding
import Logic.Player
import Constants
import Model

-- Updating the ghost
updateGhosts :: GameState -> IO GameState
updateGhosts gstate = do 
                      newGhostlist <- mapM (updateGhost gstate) (ghosts gstate)
                      return (gstate {ghosts = newGhostlist})

updateGhost :: GameState -> Player -> IO Player
updateGhost gstate g@(Ghost _ _ Frightened _ _ _) = updateGhostByFrightened gstate g
updateGhost gstate g@(Ghost _ _ Retreat _ _ _) = return (updateGhostByRetreat gstate g) 
updateGhost gstate g@(Ghost _ _ Idle _ _ _) = return (updateGhostByIdle gstate g)           -- This one needs to be made explicitly clear
updateGhost gstate g@(Ghost _ _ _ timestamp _ _ )
            | (time gstate) >= (timestamp) = return (flipGhostStateHunt gstate g)
            | otherwise = return (updateGhost2 gstate g)

-- update the players with the state Chase Scatter, ToScatterPlace and Idle. since at updateGhost first needed to be checked if the timing condition was true, the next part of pattermatching is here
updateGhost2::GameState -> Player -> Player
updateGhost2 gstate g@(Ghost _ _ (Scatter r) _ _ _) = updateGhostByScatter gstate g
updateGhost2 gstate g@(Ghost _ _ ToScatterPlace _ _ _) = updateGhostByScatterPlace gstate g
updateGhost2 gstate g@(Ghost _ _ Chase _ _ _) = updateGhostByChase gstate g
updateGhost2 gstate ghost = ghost


updateGhostByScatter :: GameState -> Player -> Player
updateGhostByScatter gstate ghost = ghost{position=pos, state = (Scatter route)}
  where (pos, route) = getNewDirPos ghost gstate

updateGhostByScatterPlace :: GameState -> Player -> Player
updateGhostByScatterPlace gstate ghost@(Ghost gPos color gState@ToScatterPlace _ _ _) = ghost{position = pos, state=state}
  where
    (pos, state) = nextPosScatter gstate gPos (getScatterStart color)

updateGhostByIdle :: GameState -> Player -> Player 
updateGhostByIdle gstate ghost@(Ghost _ RED _ _ _ _) = updateIdleGhost ghost gstate 0
updateGhostByIdle gstate ghost@(Ghost _ PINK _ _ _ _) = updateIdleGhost ghost gstate 0
updateGhostByIdle gstate ghost@(Ghost _ ORANGE _ _ _ _) = updateIdleGhost ghost gstate 60
updateGhostByIdle gstate ghost@(Ghost _ CYAN _ _ _ _) = updateIdleGhost ghost gstate 30


updateGhostByChase :: GameState -> Player -> Player 
updateGhostByChase gstate ghost@(Ghost (gx, gy) ORANGE _ _ _ _) = 
  if distance > 8 then ghost{position = newPosRed} else ghost{state = ToScatterPlace} -- follow reds tacic
  where
    (px, py) = (position (player gstate)) -- get position of pacman
    distance = sqrt (fromIntegral ((px - gx)^2 + (py - gy)^2 )) -- calculate distance to Pacman (first convert to integral, then sqrt operation
    newPosRed = position (updateGhostByChase gstate ghost{gColor = RED}) -- Follow the same tactic as red, therefore be for one step a Red ghost

updateGhostByChase gstate ghost@(Ghost gPos CYAN _ _ _ _) = ghost{position = loc}
  where
    (px, py) = position (player (updatePosition (updatePosition gstate) ) ) -- update the player position with two steps
    ((Ghost (rx,ry) _ _ _ _ _): gs) = ghosts gstate -- get redghost and its position
    destination = ( 2 * px - rx, 2 * py - ry ) -- calculate the position where cyan needs to go to
    loc = updateGposAstar gstate gPos destination -- calculate route and go one step into that direction

updateGhostByChase gstate ghost@(Ghost gPos RED _ _ _ _) = ghost{position = loc}
  where
    loc = updateGposAstar  gstate gPos (position (player gstate))

updateGhostByChase gstate ghost@(Ghost gPos PINK _ _ _ _) = ghost{position = loc}
  where
    newPlayerPos = position (player (updatePosition (updatePosition (updatePosition (updatePosition gstate) ) ) ))  -- update the player position with four steps
    loc = updateGposAstar gstate gPos newPlayerPos

updateGhostByChase _ g = g

-- Let the ghost walk to the return base. Switch state to Idle if destination has been reached
updateGhostByRetreat :: GameState -> Player -> Player
updateGhostByRetreat gstate g@(Ghost gPos color gState t s d) = 
  let (r:o:p:c:xs) = map position (ghosts runningGameState)  -- get the begin position of all different color ghosts
      newPos = case color of
                RED -> updateGposAstar gstate gPos r
                PINK -> updateGposAstar gstate gPos  p
                ORANGE -> updateGposAstar gstate gPos  o
                CYAN -> updateGposAstar gstate gPos  c  in 
  if gPos == newPos then g{state = Idle} else g{position = newPos}


flipGhostStateHunt::GameState -> Player->Player
flipGhostStateHunt gstate (Ghost gPos gColor Chase timestamp sequenc direction) = newGhost
  where
    newTimeOutTime = getTimeOutTime sequenc
    newSequenceNumber = sequenc+1
    newGhost =  setGhostToState ToScatterPlace ((time gstate) + fromIntegral newTimeOutTime) (Ghost gPos gColor Chase timestamp newSequenceNumber direction)

flipGhostStateHunt gstate g@(Ghost gPos gColor (Scatter _) timestamp sequenc direction) = newGhost
  where
    newTimeOutTime = getTimeOutTime sequenc
    newSequenceNumber = sequenc+1
    newGhost =  setGhostToState Chase ((time gstate) + fromIntegral newTimeOutTime) (Ghost gPos gColor Chase timestamp newSequenceNumber direction)
flipGhostStateHunt gstate ghost = ghost


updateGhostByFrightened:: GameState -> Player -> IO Player
updateGhostByFrightened gstate ghost@(Ghost _ _ Frightened timestamp _ _) 
    | (time gstate) >= (timestamp) = return (setGhostToState Chase (time gstate) ghost)
    | otherwise = updateRandomPos gstate ghost
updateGhostByFrightened gstate ghost = return ghost

rng :: Int -> IO Int
rng upper = randomRIO (0,upper-1)

randomElementFromList :: [a] -> IO a
randomElementFromList list = do
  r <- rng (length list)
  return $ list !! r

updateRandomPos :: GameState -> Player -> IO Player
updateRandomPos gstate g@(Ghost curr _ _ _ _ direction) = 
  let new_pos = calculateNextPosition curr direction in
    case positionWalkable new_pos gstate of
      True -> return g{position = new_pos}
      _    -> do 
                newDir <- randomElementFromList [ x | x <- [UP, DOWN, RIGHT, LEFT], x /= direction ] 
                return g{gDirection = newDir}


pickNewRandomPosition :: Player -> GameState -> Player
pickNewRandomPosition (Ghost gPos gColor Frightened timestamp sequenc direction) gstate = (Ghost newPos gColor Frightened timestamp sequenc direction)
  where
    newPosOptions = (getWalkableNeighborTilePositions gstate gPos) ++ [gPos]
    amountOfOptions = length newPosOptions
    newPos = unsafePerformIO $ randomElementFromList newPosOptions

-- update the position with one step into the shortest path to the destination
updateGposAstar :: GameState -> (Int, Int) -> (Int, Int) -> (Int, Int)
updateGposAstar gstate start end = case findPath gstate start end of
                                    Just (steps, (x : xs)) -> x
                                    _ -> start

-- calculates the new position of the route the scatter ghosts need to take
nextPosScatter::GameState -> (Int, Int) -> (Int, Int) -> ((Int, Int), GhostState)
nextPosScatter gstate start des = let new_pos = updateGposAstar gstate start des in
                                      case new_pos == start of
                                        True -> (start, (Scatter []))
                                        _    -> (new_pos, ToScatterPlace)

-- for the scatter ghosts: walk the route. If you can take the next turn, take it and update your directionlist
getNewDirPos:: Player -> GameState -> ((Int,Int), [Direction])
getNewDirPos ghost@(Ghost gPos gColor (Scatter route) _ _ _) gstate = 
  case route of
    (x:y:xs) -> let new_pos = calculateNextPosition gPos y in
                  case positionWalkable new_pos gstate of
                    True -> (new_pos, y:xs)
                    False -> let new_pos1 = calculateNextPosition gPos x in
                              case positionWalkable new_pos1 gstate of
                                True -> (new_pos1, (x:y:xs) )
                                _    -> (gPos, (x:y:xs) )
    [x]      -> let new_pos = calculateNextPosition gPos x in
                  case positionWalkable new_pos gstate of
                    True -> (new_pos, [x])
                    _    -> (gPos, getTotalRoute ghost)
    _        -> (gPos, getTotalRoute ghost)

getNewDirPos ghost@(Ghost gPos _ _ _ _ _) gstate = (gPos, [])

-- Check if Idle ghost can be activated
updateIdleGhost:: Player -> GameState -> Int -> Player
updateIdleGhost ghost@(Ghost gPos gColor Idle timestamp sequence direction) gstate minCoins =
  if (consumablesTotal gstate) - (consumablesLeft gstate) >= minCoins then ghost{state = Chase} else ghost
updateIdleGhost g _ _ = g

