module Day18.Mod where

import Data.List (nub)
import Data.Map (fromListWith, toList)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod

parseInt :: Parser Int
parseInt = read <$> some numberChar <?> "integer"

type Point = (Int, Int, Int)

neighbors :: Point -> [Point]
neighbors (x, y, z) =
  [ -- x
    (x + 1, y, z),
    (x - 1, y, z),
    -- y
    (x, y + 1, z),
    (x, y - 1, z),
    -- z
    (x, y, z + 1),
    (x, y, z - 1)
  ]

parsePoint :: Parser Point
parsePoint = do
  x <- parseInt
  _ <- string ","
  y <- parseInt
  _ <- string ","
  z <- parseInt
  pure (x, y, z)

type Cubes = Set Point

-- walkCubes :: Cubes -> [Point] -> Int
-- walkCubes _ [] = 0
-- walkCubes cubes (curr:remaining) = 0
--  where

countSides :: Cubes -> Point -> Int
countSides cubes p = 6 - numCovered
  where
    numCovered = Set.size (Set.intersection (Set.fromList $ neighbors p) cubes)

part1 :: IO ()
part1 = do
  print "part1"
  input <- Set.fromList <$> readInputLinesParser parsePoint
  print $ sum (map (countSides input) (Set.toList input))
  return ()

getOpenPoints :: Cubes -> Point -> [Point]
getOpenPoints cube p = filter (`Set.notMember` cube) (neighbors p)

-- frequency :: (Ord a) => [a] -> [(a, Int)]
-- frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- getOpenPointsCount :: Cubes -> [(Point, Int)]
-- getOpenPointsCount c = frequency $ concatMap (getOpenPoints c) (Set.toList c)

validPoint :: Point -> Bool
validPoint (x, y, z) = all (isInRange (-1) 25) [x, y, z]

walkCubes :: Cubes -> [Point] -> Set Point -> Set Point
walkCubes _ [] seen = seen
walkCubes cubes (x : xs) seen = walkCubes cubes remaining newSeen
  where
    newSeen = Set.insert x seen
    sides = Set.fromList $ neighbors x
    newPoints = Set.filter validPoint $ Set.difference (Set.difference sides cubes) newSeen
    remaining = xs ++ Set.toList (newPoints)

countSidesP2 :: Set Point -> Point -> Int
countSidesP2 seen p = Set.size (Set.intersection (Set.fromList $ neighbors p) seen)

part2 :: IO ()
part2 = do
  print "part2"
  input <- Set.fromList <$> readInputLinesParser parsePoint
  let seen = walkCubes input [(-1, -1, -1)] Set.empty
  print $ seen
  print $ sum (map (countSidesP2 seen) (Set.toList input))
  -- let freq = getOpenPointsCount input
  -- let res = sum [x | (_, x) <- freq, x /= 6]
  -- print $ freq
  -- print res
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
