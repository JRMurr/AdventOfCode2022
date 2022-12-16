module Day15.Mod where

import Data.List (find)
import Data.Maybe (isNothing)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
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

-- get all points less than the manhattan dist btwn the sensor and beacon at the provided row
getPointsInRadius :: Int -> SensorBecaon -> Set Int
getPointsInRadius row (sc@(C sy sx), bc) = Set.fromDistinctAscList [sx + x | x <- range]
  where
    md = manhattan sc bc
    d = md - abs (row - sy)
    range = if d < 0 then [] else [-d .. d]

getAllNonBeaconPoints :: Int -> [SensorBecaon] -> Set Int
getAllNonBeaconPoints row sb = allPoints \\ beaconsInRow
  where
    allSets = map (getPointsInRadius row) sb
    allPoints = foldl Set.union Set.empty allSets
    beaconsInRow = Set.fromList [bx | (_, C by bx) <- sb, by == row]

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesMapper (parseThrow parseLine)
  print $ Set.size $ getAllNonBeaconPoints 2000000 input
  return ()

-- the x,y point must be 1 unit outside the range of the existing sensors to be unique

-- get the perimmiter points of a diamond of given size
getPointsAtDist :: Int -> [Coord]
getPointsAtDist dist = C 0 dist : C 0 (-dist) : C (-dist) 0 : C dist 0 : [C (x * mulx) (y * muly) | ((x, y), (mulx, muly)) <- cartProd points muls]
  where
    points = [(i, dist - i) | i <- [1 .. dist]]
    muls = let nums = [-1, 1] in [(x, y) | x <- nums, y <- nums]

getPossibleForPair :: Int -> SensorBecaon -> [Coord]
getPossibleForPair maxCoordVal (sc, bc) = filter (\(C x y) -> validCoord x && validCoord y) possiblePoints
  where
    validCoord = isInRange 0 maxCoordVal
    md = manhattan sc bc
    possiblePoints = map (addCoord sc) $ getPointsAtDist (md + 1)

isInside :: Coord -> SensorBecaon -> Bool
isInside c (sc, bc) = dist <= radius
  where
    radius = manhattan sc bc
    dist = manhattan sc c

findPoint :: Int -> [SensorBecaon] -> Maybe Coord
findPoint maxCoordVal pairs = head $ dropWhile isNothing $ [find isValid p | p <- allPossible] -- need to be weird for laziness
  where
    getPossibleForPair' = getPossibleForPair maxCoordVal
    allPossible = map getPossibleForPair' pairs
    isValid c = not (any (isInside c) pairs)

getTuning :: Int -> [SensorBecaon] -> Int
getTuning maxCoordVal pairs = x * 4000000 + y
  where
    Just (C y x) = findPoint maxCoordVal pairs

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper (parseThrow parseLine)
  print $ getTuning 4000000 input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
