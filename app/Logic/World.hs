module Logic.World where
import Model

import Constants


-- * m
updateWorld::GameState->GameState
updateWorld gstate = detectingTilePlayerInteraction $ detectPlayerOnCoin gstate


-- ** Detecting if a player picked up a coin
detectPlayerOnCoin::GameState->GameState
detectPlayerOnCoin gstate = gstate {ghosts = getGhostFrightened gstate}

-- * Detecting if the player is on top of a frightened ghost
getGhostFrightened::GameState->Ghosts
getGhostFrightened gstate  | RUNNING /= (runningState gstate) = g
                           | isCoin playerTile == True = freightGhosts
                           | otherwise = g
                           where
                             g = ghosts gstate
                             playerPos = position $ player gstate
                             playerTile = getTileByPosition gstate playerPos
                             freightGhosts = setGhostsToState Frightened g $ (frightTime + time gstate)


-- ** Updating the amount of available consumables
setConsumablesLeft :: GameState -> Int -> GameState
setConsumablesLeft gstate dots = gstate {consumablesLeft = dots}

-- ** Decreasing the amount of consumables
decreaseConsumableCount :: GameState -> GameState
decreaseConsumableCount gstate = setConsumablesLeft gstate $ (consumablesLeft gstate) - 1


-- ** Detecting if a player has interaction with a consumable tile.
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


-- Adding a new tile to the world
mergeWorldRTile::Tile-> ([Tile], [Tile])->World
mergeWorldRTile tile (x, y) = x++ [tile] ++ y

-- Splitting the world to specific tile rangers
splitWorldForUpdate::GameState -> Int -> ([Tile], [Tile])
splitWorldForUpdate gstate index = popOldTile (splitAt (index) $ world gstate)

-- Removing an old tile from the world
popOldTile::([Tile], [Tile]) -> ([Tile], [Tile])
popOldTile (x, y) = (x, tail y)
