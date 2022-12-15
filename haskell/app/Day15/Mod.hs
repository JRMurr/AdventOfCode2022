{-# LANGUAGE LambdaCase #-}

module Day15.Mod where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe
import qualified Debug.Trace as Debug
import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Utils.Coords
import Utils.Mod

-- parse signed int
parseInt :: Parser Int
parseInt = label "integer" $ read <$> (some numberChar <|> ((:) <$> char '-' <*> some numberChar))

-- x=<int>, y=<int>
parseCoord :: Parser Coord
parseCoord = label "coord" $ do
  _ <- string "x="
  x <- parseInt
  _ <- string ", y="
  y <- parseInt
  pure (C y x)

-- could make this a record but nah
-- first is sensor second is closest beacon
type SensorBecaon = (Coord, Coord)

parseLine :: Parser SensorBecaon
parseLine = label "sensor and becaon" $ do
  _ <- string "Sensor at "
  sensor <- parseCoord
  _ <- string ": closest beacon is at "
  beacon <- parseCoord
  pure (sensor, beacon)

-- each coord can be a sensort, a becaon, empty (since we know no beacon could be closer), or not checked yet
data CoordVal = Sensor | Becaon | Empty | NotChecked deriving (Show, Eq)

type SensorMap = Map Coord CoordVal

-- given a size calc the points inside the diamond
-- TODO: maybe make this recursive and memoize it?
-- might still need to cache it
getDiamondCoord :: Int -> [Coord]
getDiamondCoord size = origin : (leftSide ++ rightSide)
  where
    -- we only need to figure out one triangle then roate/flip each point
    -- which is just size southwest diag lines + the origin
    getDiagLines n = pointsInLine (addCoord origin (C 0 n)) (addCoord origin (C n 0))
    lowerRightTriangle = concatMap getDiagLines [1 .. size]
    upperRightTriag = mapMaybe (\(C y x) -> if y == 0 then Nothing else Just $ C (-y) x) lowerRightTriangle
    rightSide = lowerRightTriangle ++ upperRightTriag
    leftSide = mapMaybe (\(C y x) -> if x == 0 then Nothing else Just $ C y (-x)) rightSide

-- TODO: instead of this, maybe first insert all the pairs
-- then count the number of points on the target row have a manhatten <= dist we calc
handleSensorBeacon :: SensorMap -> SensorBecaon -> SensorMap
handleSensorBeacon sMap (sc, bc) = insertList pairMap [(x, Empty) | x <- emptyPoints]
  where
    pairMap = insertList sMap [(sc, Sensor), (bc, Becaon)]
    dist = manhattan sc bc
    diamond = getDiamondCoord dist
    emptyPoints = Debug.trace ("(dist, sc, bc): " ++ show (dist, sc, bc)) $ filter (`Map.notMember` pairMap) $ map (addCoord sc) diamond

handleAllPairs :: [SensorBecaon] -> SensorMap
handleAllPairs = foldl handleSensorBeacon Map.empty

drawMap :: SensorMap -> String
drawMap =
  drawCoordsGen
    ( \case
        Sensor -> 'S'
        Becaon -> 'B'
        Empty -> '#'
        NotChecked -> '.'
    )
    NotChecked

getRow :: SensorMap -> Int -> [CoordVal]
getRow sMap row = map (sMap !) coords
  where
    coords = filter (\(C y _) -> y == row) (Map.keys sMap)

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesMapper (parseThrow parseLine)
  -- print input
  -- print $ length $ getDiamondCoord 3
  let res = handleAllPairs input
  -- putStrLn $ drawMap (handleSensorBeacon Map.empty (C (-18) 2, C (-15) (-2)))
  -- putStrLn $ drawMap res
  print $ count (/= Becaon) $ getRow res 2000000
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
