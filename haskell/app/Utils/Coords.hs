{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Coords where

import qualified Control.Arrow as Data.Bifunctor
import Data.Char (digitToInt)
import Data.Foldable
import qualified Data.Foldable as Set
import Data.Hashable
import Data.Ix
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import GHC.Arr
import GHC.Generics (Generic)

data Coord = C !Int !Int
  deriving (Read, Show, Ord, Eq, Generic)

instance Hashable Coord

instance Ix Coord where
  unsafeIndex (C lorow locol, C _hirow hicol) (C row col) =
    (row - lorow) * (hicol - locol + 1) + (col - locol)

  inRange (C lorow locol, C hirow hicol) (C row col) =
    lorow <= row
      && row <= hirow
      && locol <= col
      && col <= hicol

  range (C lorow locol, C hirow hicol) =
    [C row col | row <- [lorow .. hirow], col <- [locol .. hicol]]

newtype Dir = Dir Coord deriving (Show)

class AddDir a where
  addDir :: a -> Dir -> a

instance AddDir Coord where
  addDir :: Coord -> Dir -> Coord
  addDir c (Dir d) = addCoord c d

getDirTowards :: Coord -> Coord -> Dir
getDirTowards end start
  | start == end = error $ "start == end: " ++ show (start, end)
  -- above states
  | start `isAbove` end && start `isRight` end = Dir (addCoord north east)
  | start `isAbove` end && start `isLeft` end = Dir (addCoord north west)
  | start `isAbove` end && start `sameCol` end = Dir north
  -- below states
  | start `isBelow` end && start `isRight` end = Dir (addCoord south east)
  | start `isBelow` end && start `isLeft` end = Dir (addCoord south west)
  | start `isBelow` end && start `sameCol` end = Dir south
  -- same row states
  | start `sameRow` end && start `isRight` end = Dir east
  | start `sameRow` end && start `isLeft` end = Dir west
  -- error
  | otherwise = error $ "invalid start end: " ++ show (start, end)

-- Get all points in the line from start to end
pointsInLine :: Coord -> Coord -> [Coord]
pointsInLine start end
  | start == end = []
  | otherwise = let (Dir d) = getDirTowards start end in end : takeWhile (/= end) (iterate (addCoord d) start)

coordRow, coordCol :: Coord -> Int
coordRow (C row _) = row
coordCol (C _ col) = col

addRow, addCol :: Int -> Coord -> Coord
addRow n (C r c) = C (r + n) c
addCol n (C r c) = C r (c + n)

-- default ordering compares rows
compareCols :: Coord -> Coord -> Ordering
compareCols c1 c2 = coordCol c1 `compare` coordCol c2

above, below, left, right :: Coord -> Coord
above (C y x) = C (y - 1) x
below (C y x) = C (y + 1) x
left (C y x) = C y (x - 1)
right (C y x) = C y (x + 1)

northWest, northEast, southWest, southEast :: Coord -> Coord
northWest = above . left
northEast = above . right
southWest = below . left
southEast = below . right

isAbove, isBelow, isLeft, isRight, sameRow, sameCol :: Coord -> Coord -> Bool
isAbove (C y1 _) (C y2 _) = y1 < y2
isBelow (C y1 _) (C y2 _) = y1 > y2
isLeft (C _ x1) (C _ x2) = x1 < x2
isRight (C _ x1) (C _ x2) = x1 > x2
sameRow (C y1 _) (C y2 _) = y1 == y2
sameCol (C _ x1) (C _ x2) = x1 == x2

turnLeft, turnRight, turnAround :: Coord -> Coord
turnLeft (C y x) = C (-x) y
turnRight (C y x) = C x (-y)
turnAround (C y x) = C (-y) (-x)

-- | Compute the Manhattan distance between two coordinates
manhattan :: Coord -> Coord -> Int
manhattan (C x y) (C u v) = abs (x - u) + abs (y - v)

-- | Compute the 4 cardinal neighbors of a coordinate: north, south, east, west
cardinal :: Coord -> [Coord]
cardinal c = c `seq` [above c, left c, right c, below c]

-- | Compute the 8 cardinal neighbors and diagonal neighbors
neighbors :: Coord -> [Coord]
neighbors c =
  c `seq`
    [ above c,
      left c,
      right c,
      below c,
      above (left c),
      above (right c),
      below (left c),
      below (right c)
    ]

neighborsCardinal :: Coord -> [Coord]
neighborsCardinal c =
  c `seq`
    [ above c,
      left c,
      right c,
      below c
    ]

-- | Given a collection of cords, get the min and max cords that would contain all the cords in the collection
boundingBox :: Foldable t => t Coord -> Maybe (Coord, Coord)
boundingBox t =
  case toList t of
    [] -> Nothing
    C y x : cs -> go y x y x cs
  where
    go loy lox hiy hix [] = Just (C loy lox, C hiy hix)
    go loy lox hiy hix (C y x : cs) = go (min loy y) (min lox x) (max hiy y) (max hix x) cs

origin :: Coord
origin = C 0 0

north, south, east, west :: Coord
north = C (-1) 0
south = C 1 0
east = C 0 1
west = C 0 (-1)

cardinalDirs :: [Coord]
cardinalDirs = [north, south, east, west]

addCoord :: Coord -> Coord -> Coord
addCoord (C y x) (C v u) = C (y + v) (x + u)

subCoord :: Coord -> Coord -> Coord
subCoord (C y x) (C v u) = C (v - y) (u - x)

cordAngle :: Coord -> Double
cordAngle (C y x) = atan2 (-fromIntegral x) (fromIntegral y)

-- drawing funcs

drawCoordSet :: Set Coord -> String
drawCoordSet s = drawCoordsGen id '.' $ Map.fromList [(c, '#') | c <- Set.toList s]

drawCoords :: Map Coord Char -> String
drawCoords = drawCoordsGen id ' '

drawCoords' :: Char -> Map Coord Char -> String
drawCoords' = drawCoordsGen id

drawCoordsGen :: (t -> Char) -> t -> Map Coord t -> String
drawCoordsGen toChar defaultChar pixels = unlines [[pixel (C y x) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    pixel c = toChar $ Map.findWithDefault defaultChar c pixels
    Just (C miny minx, C maxy maxx) = boundingBox (Map.keys pixels)

-- | Read cords for each char in a list of strings. 0,0 would be the first char in the first string
coordLines :: [[b]] -> [(Coord, b)]
coordLines rows = [(C y x, z) | (y, row) <- zip [0 ..] rows, (x, z) <- zip [0 ..] row]

coordLinesInt :: [String] -> [(Coord, Int)]
coordLinesInt s = map (Data.Bifunctor.second digitToInt) $ coordLines s

-- | Get points in a given direction from the starting point that are in the map
pointsAlongLine :: Coord -> Map Coord a -> Coord -> [a]
pointsAlongLine c tm dir = catMaybes $ takeWhile isJust $ map (`Map.lookup` tm) $ tail $ iterate (addCoord dir) c
