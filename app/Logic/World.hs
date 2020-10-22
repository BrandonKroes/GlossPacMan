module Logic.World where
import Model

import Constants



updateWorld::GameState->GameState
updateWorld gstate = detectingTilePlayerInteraction $ detectPlayerOnCoin gstate


-- Detecting if a player picket up a coin
detectPlayerOnCoin::GameState->GameState
detectPlayerOnCoin gstate = gstate {ghosts = getGhostFrightened gstate}



getGhostFrightened::GameState->Ghosts
getGhostFrightened gstate  | RUNNING /= (runningState gstate) = g
                           | isCoin playerTile == True = freightGhosts
                           | otherwise = g
                           where
                             g = ghosts gstate
                             playerPos = position $ player gstate
                             playerTile = getTileByPosition gstate playerPos
                             freightGhosts = setGhostsToState Frightened g $ (frightTime + time gstate)


-- Set imgCoin count
setConsumablesLeft :: GameState -> Int -> GameState
setConsumablesLeft gstate dots = gstate {consumablesLeft = dots}

-- Decrease imgCoin count
decreaseConsumableCount :: GameState -> GameState
decreaseConsumableCount gstate = setConsumablesLeft gstate $ (consumablesLeft gstate) - 1

detectingTilePlayerInteraction::GameState->GameState
detectingTilePlayerInteraction gstate
  |isConsumable tile == True = decreaseConsumableCount $ gstate {world=nWorld}
  |isConsumable tile == False = gstate {world=oWorld}
  |otherwise = gstate
    where
      tPosition = (position $ player gstate)
      tile = getTileByPosition gstate tPosition
      replacementTile = setTileField tile Empty
      nWorld = mergeWorldRTile replacementTile
               (splitWorldForUpdate gstate $
                getTileIndexByPosition gstate
                tPosition)
      oWorld = world gstate


mergeWorldRTile::Tile-> ([Tile], [Tile])->World
mergeWorldRTile tile (x, y) = x++ [tile] ++ y


splitWorldForUpdate::GameState -> Int -> ([Tile], [Tile])
splitWorldForUpdate gstate index = popOldTile (splitAt (index) $ world gstate)

popOldTile::([Tile], [Tile]) -> ([Tile], [Tile])
popOldTile (x, y) = (x, tail y)
