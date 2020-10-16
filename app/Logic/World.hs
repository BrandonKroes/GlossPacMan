module Logic.World where
import Model




updateWorld::GameState->GameState
updateWorld gstate = detectingTilePlayerInteraction gstate

-- Set coin count
setDotCount :: GameState -> Int -> GameState
setDotCount gstate dots = gstate {dotsLeft = dots}

-- Decrease coin count
decreaseDotCount :: GameState -> GameState
decreaseDotCount gstate = setDotCount gstate $ (dotsLeft gstate) - 1

detectingTilePlayerInteraction::GameState->GameState
detectingTilePlayerInteraction gstate
  |isConsumable tile== True = gstate {world=nWorld}
  |isConsumable tile== False = gstate {world=oWorld}
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
