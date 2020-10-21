module Logic.PathFinding where

import Algorithm.Search

import Model

taxicabNeighbors :: (Int, Int) -> [(Int, Int)]
taxicabNeighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]

isWall gstate (x,y) = case getTileByPosition gstate (x,y) of
                          NotWalkable _ _ -> True
                          _               -> False

taxicabDistance :: (Int, Int) -> (Int, Int) -> Int
taxicabDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

findPath :: GameState -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
findPath gstate start end =
  let next = taxicabNeighbors
      cost = taxicabDistance
      remaining = (taxicabDistance end)
  in aStar (next `pruning` (isWall gstate)) cost remaining (== end) start