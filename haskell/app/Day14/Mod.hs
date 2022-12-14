module Day14.Mod where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe)
import qualified Debug.Trace as Debug
import Utils.Coords
import Utils.Mod

type RockPath = [Coord]

parseLine :: String -> RockPath
parseLine points = map parseCoord (splitOn " -> " points)
  where
    parseCoord s = let [col, row] = map read $ splitOn "," s in C row col

data Block = Sand | Block | Air deriving (Eq)

instance Show Block where
  show Sand = "o"
  show Block = "#"
  show Air = "."

type Blocks = Map Coord Block

fillRocks :: Blocks -> RockPath -> Blocks
fillRocks bMap path = foldl (\mp k -> Map.insert k Block mp) bMap linePoints
  where
    pairs = map tuplify2 (chunkOverlap 2 path)
    linePoints = concatMap (uncurry pointsInLine) pairs

-- get bounding box for map then fill all missing points with air
fillAir :: Blocks -> Blocks
fillAir bMap = Map.fromList allPoints
  where
    Just (C miny minx, C maxy maxx) = boundingBox (Map.keys bMap)
    getPoint c = (c, Map.findWithDefault Air c bMap)
    allPoints = [getPoint (C y x) | x <- [minx .. maxx], y <- [miny .. maxy]]

-- get all coords with air and rocks filled
getMap :: [RockPath] -> Blocks
getMap paths = fillAir $ foldl fillRocks Map.empty paths

sandStartCoord :: Coord
sandStartCoord = C 0 500

getPointsBelow :: Coord -> [Coord]
getPointsBelow = iterate (addCoord south)

sandVertLine :: [Coord]
sandVertLine = getPointsBelow sandStartCoord

getFirstNonAirBelow :: Blocks -> Coord -> Coord
getFirstNonAirBelow bMap start = above $ head $ dropWhile (\c -> Map.findWithDefault Air c bMap == Air) (getPointsBelow start)

drawMap :: Blocks -> String
drawMap = drawCoordsGen (head . show) Air

isBelowAll :: Blocks -> Coord -> Bool
isBelowAll bMap (C y _) = y > maxy
  where
    Just (_, C maxy _) = boundingBox (Map.keys bMap)

belowCoords :: Coord -> [Coord]
belowCoords c = c `seq` [below c, southWest c, southEast c]

-- given a point a sand is resting above see where it will rest
-- return nothing if goes to void
findResting :: Blocks -> Coord -> Maybe Coord
findResting bMap c
  | isBelowAll bMap c = Nothing
  -- \| firstNonAir `isBelow` c = findResting bMap firstNonAir
  | otherwise = case find (\bc -> Map.findWithDefault Air bc bMap == Air) (belowCoords c) of
      -- there is a path for air so follow it
      Just p -> findResting bMap p
      -- all blocked so rest here
      Nothing -> Just c
  where
    firstNonAir = getFirstNonAirBelow bMap c

dropSand :: Blocks -> Maybe Blocks
dropSand bMap = fmap (\c -> Map.insert c Sand bMap) (findResting bMap sandStartCoord)

-- where
-- belowPoint = above $ head $ dropWhile (\c -> Map.findWithDefault Air c bMap == Air) sandVertLine
-- restPoint = findResting bMap sandStartCoord

dropSandTillAbyss :: Blocks -> Int -> (Int, Blocks)
dropSandTillAbyss bMap sandCount = case dropSand bMap of
  Nothing -> (sandCount, bMap)
  Just newMap -> Debug.trace ("count: " ++ show sandCount) $ dropSandTillAbyss newMap (sandCount + 1)

dropSandHappy :: Blocks -> Blocks
dropSandHappy b = fromJust $ dropSand b

dropSandXTimes :: Blocks -> Int -> Blocks
dropSandXTimes bMap n = iterate dropSandHappy bMap !! n

part1 :: IO ()
part1 = do
  print "part1"
  input <- getMap <$> readInputLinesMapper parseLine
  let (sandCount, finalMap) = dropSandTillAbyss input 0
  -- putStrLn $ drawMap $ dropSandXTimes input 3
  putStrLn $ drawMap $ finalMap
  print sandCount
  return ()

-- TODO
-- if lands at point with west and east air and sw and se blocks, can fill west and east
-- if lands with left and right neighbors next will land on top
-- maybe memoize the boundingBox call?

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper parseLine
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
