module Day08.Mod where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils.Coords (Coord, cardinalDirs, coordLinesInt, pointsAlongLine)
import Utils.Mod

type Trees = [(Coord, Int)]

type TreeMap = Map Coord Int

isVisAlongLine :: Coord -> TreeMap -> Coord -> Bool
isVisAlongLine c tm dir = all (< currHeight) dirPoints
  where
    currHeight = tm ! c
    dirPoints = pointsAlongLine c tm dir

isVis :: Coord -> TreeMap -> Bool
isVis c tm = any (isVisAlongLine c tm) cardinalDirs

part1 :: IO ()
part1 = do
  print "part1"
  input <- Map.fromList . coordLinesInt <$> readInputLinesMapper id
  print $ count (`isVis` input) (Map.keys input)
  return ()

numTreeSeenInLine :: Coord -> TreeMap -> Coord -> Int
numTreeSeenInLine c tm dir = seenTrees
  where
    currHeight = tm ! c
    dirPoints = pointsAlongLine c tm dir
    (lessThanTrees, greaterEqualTree) = span (< currHeight) dirPoints
    seenTrees =
      length lessThanTrees
        + (if not (null greaterEqualTree) then 1 else 0)

getScore :: Coord -> TreeMap -> Int
getScore c tm = product $ map (numTreeSeenInLine c tm) cardinalDirs

part2 :: IO ()
part2 = do
  print "part2"
  input <- Map.fromList . coordLinesInt <$> readInputLinesMapper id
  print input
  print $ maximum $ map (`getScore` input) (Map.keys input)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
