module Day08.Mod where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Utils.Coords (Coord, addCoord, cardinalDirs, coordLinesInt)
import Utils.Mod

type Trees = [(Coord, Int)]

type TreeMap = Map Coord Int

isVisAlongLine :: Coord -> TreeMap -> Coord -> Bool
isVisAlongLine c tm dir = all (< currHeight) pointsAlongLine
  where
    currHeight = tm ! c
    pointsAlongLine = catMaybes $ takeWhile isJust $ map (`Map.lookup` tm) $ tail $ iterate (addCoord dir) c

isVis :: Coord -> TreeMap -> Bool
isVis c tm = any (isVisAlongLine c tm) cardinalDirs

part1 :: IO ()
part1 = do
  print "part1"
  input <- Map.fromList . coordLinesInt <$> readInputLinesMapper id
  print $ count (`isVis` input) (Map.keys input)
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
