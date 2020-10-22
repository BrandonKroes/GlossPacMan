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
updateGhosts gstate = gstate {ghosts = map (updateGhost gstate) $ ghosts gstate}

updateGhost :: GameState -> Player -> Player
updateGhost gstate (Ghost gPos gColor Frightened timestamp sequenc) = updateGhostByFrightened (Ghost gPos gColor Frightened timestamp sequenc) gstate
updateGhost gstate (Ghost gPos gColor Scatter timestamp sequenc)    = Ghost gPos gColor Scatter timestamp sequenc
updateGhost gstate (Ghost gPos gColor Chase timestamp sequenc)      = updateGhostByChase (Ghost gPos gColor Chase timestamp sequenc) gstate
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
    newPosOptions = getWalkableNeighborTilePositions gstate gPos
    amountOfOptions = length newPosOptions
    newPos = unsafePerformIO $ randomElementFromList newPosOptions

-- Nested guarding isn't a thing in haskell, so abstracting it.
updateGhostByChase::Player -> GameState -> Player
updateGhostByChase (Ghost gPos RED gState timestamp sequenc) gstate = getRedGhost   (Ghost gPos RED gState timestamp sequenc) gstate
updateGhostByChase (Ghost gPos PINK gState timestamp sequenc) gstate = getPinkGhost (Ghost gPos PINK gState timestamp sequenc) gstate
updateGhostByChase (Ghost gPos ORANGE gState timestamp sequenc) gstate = getOrangeGhost (Ghost gPos ORANGE gState timestamp sequenc) gstate
updateGhostByChase (Ghost gPos CYAN gState timestamp sequenc) gstate = getCyanGhost (Ghost gPos CYAN gState timestamp sequenc) gstate
updateGhostByChase ghost gstate = ghost

updateGposAstar :: GameState -> (Int, Int) -> (Int, Int) -> (Int, Int)
updateGposAstar gstate start end = case findPath gstate start end of
                                    Just (steps, (x : xs)) -> x
                                    _ -> start

getOrangeGhost::Player -> GameState -> Player
getOrangeGhost og@(Ghost (gx, gy) gColor gState timestamp sequenc ) gstate = if distance > 8 then getRedGhost og gstate else (Ghost (gx, gy) gColor Scatter timestamp sequenc) -- follow reds tackic
  where
    (px, py) = (position (player gstate))
    distance = sqrt (fromIntegral ((px - gx)^2 + (py - gy)^2 )) -- first convert to integral, then sqrt operation


getCyanGhost::Player -> GameState -> Player
getCyanGhost (Ghost gPos gColor gState timestamp sequenc) gstate = Ghost loc gColor gState timestamp sequenc
  where
    (px, py) = position (player (updatePosition (updatePosition gstate) ) ) -- update the player position with two steps
    ((Ghost (rx,ry) _ _ _ _): gs) = ghosts gstate
    destination = ( 2 * px - rx, 2 * py - ry )
    loc = updateGposAstar gstate gPos destination

getRedGhost :: Player -> GameState -> Player
getRedGhost (Ghost gPos gColor gState timestamp sequenc) gstate = Ghost loc gColor gState timestamp sequenc
  where
    loc = updateGposAstar  gstate gPos (position (player gstate))

getPinkGhost :: Player -> GameState -> Player
getPinkGhost (Ghost gPos gColor gState timestamp sequenc) gstate = Ghost loc gColor gState timestamp sequenc
  where
    newPlayerPos = position (player (updatePosition (updatePosition (updatePosition (updatePosition gstate) ) ) ))  -- update the player position with four steps
    loc = updateGposAstar gstate gPos newPlayerPos
