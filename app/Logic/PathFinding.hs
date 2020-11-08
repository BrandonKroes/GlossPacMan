module Logic.PathFinding where

import Algorithm.Search

import Model

-- get all neighbours of this position
taxicabNeighbors :: (Int, Int) -> [(Int, Int)]
taxicabNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]

-- check if this position is a wall (aka not walkable)
isWall :: GameState -> (Int,Int) -> Bool
isWall gstate (x,y) = case getTileByPosition gstate (x,y) of
                          NotWalkable _ _ -> True
                          _               -> False

-- get the taxicab distance between the points
taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- get the shortest path from the first to the second position.
findPath :: GameState -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
findPath gstate start end =
  let next = taxicabNeighbors
      cost = taxicabDistance
      remaining = (taxicabDistance end)
  in aStar (next `pruning` (isWall gstate)) cost remaining (== end) start