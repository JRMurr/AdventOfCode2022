module Day12.Mod where

import Data.Char (isAsciiLower, ord)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import qualified Debug.Trace as Debug
import Utils.AStar (astarSearch)
import Utils.Coords
import Utils.Mod

charToHeight :: Char -> Int
charToHeight a
  | isAsciiLower a = ord a - 96
  | otherwise = error $ "invalid char: " ++ show a

type HeightMap = Map Coord Int

-- read chars, convert to heigh after getting start and end
readCoords :: [String] -> (Coord, Coord, HeightMap)
readCoords input = case (start, end, accMap) of
  (Just s, Just e, m) -> (s, e, m)
  _ -> error "sad parse"
  where
    charMap = Map.fromList . coordLines $ input
    reducer coord char (maybeStart, maybeEnd, newMap) = case char of
      'S' -> (Just coord, maybeEnd, Map.insert coord (charToHeight 'a') newMap)
      'E' -> (maybeStart, Just coord, Map.insert coord (charToHeight 'z') newMap)
      _ -> (maybeStart, maybeEnd, Map.insert coord (charToHeight char) newMap)
    (start, end, accMap) = Map.foldrWithKey reducer (Nothing, Nothing, Map.empty) charMap

getValidMoves :: HeightMap -> Coord -> [(Coord, Int)]
getValidMoves hMap curr = [(x, 1) | x <- possiblePoints]
  where
    currHeight = hMap ! curr
    -- can be at most 1 higher but any amount
    coordAllowed x = maybe False ((currHeight + 1) >=) (Map.lookup x hMap)
    possiblePoints = filter coordAllowed $ neighborsCardinal curr

callAStart :: Coord -> Coord -> HeightMap -> Maybe (Int, [Coord])
callAStart start end hMap = astarSearch start isGoal (getValidMoves hMap) (const 0)
  where
    isGoal x = x == end

callAStarP1 :: Coord -> Coord -> HeightMap -> Int
callAStarP1 start end hMap = fst . fromJust $ callAStart start end hMap

part1 :: IO ()
part1 = do
  print "part1"
  (start, end, hMap) <- readCoords <$> readInputLines
  print $ callAStarP1 start end hMap
  return ()

findShortest :: Coord -> HeightMap -> Int
findShortest end hMap = minimum $ map fst $ mapMaybe (\start -> callAStart start end hMap) validStarts
  where
    validStarts = Map.keys $ Map.filter (charToHeight 'a' ==) hMap

part2 :: IO ()
part2 = do
  print "part2"
  (_, end, hMap) <- readCoords <$> readInputLines
  print $ findShortest end hMap
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
