{-# LANGUAGE InstanceSigs #-}

module Day09.Mod where

import Control.Monad.State
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Coords (Coord, addCoord, east, neighbors, north, origin, south, west)
import Utils.Mod

newtype Head = H Coord deriving (Show, Eq, Ord)

newtype Tail = T Coord deriving (Show, Eq, Ord)

type Visited = Set Tail

newtype Dir = Dir Coord deriving (Show) -- TODO: move to coord utils?

class AddDir a where
  addDir :: a -> Dir -> a

instance AddDir Head where
  addDir :: Head -> Dir -> Head
  addDir (H h) (Dir d) = H (addCoord h d)

instance AddDir Tail where -- not needed?
  addDir :: Tail -> Dir -> Tail
  addDir (T t) (Dir d) = T (addCoord t d)

charToDir :: Char -> Dir
charToDir 'R' = Dir east
charToDir 'D' = Dir south
charToDir 'L' = Dir west
charToDir 'U' = Dir north
charToDir c = error $ "invalid char: " ++ show c

-- Read the input into a list of cardinal directions
readCommand :: String -> [Dir]
readCommand s = replicate (read amnt) (charToDir c)
  where
    [[c], amnt] = splitOn " " s

type RopeState = (Head, Tail, Visited)

type EndVal = Visited

isValidRopePos :: Head -> Tail -> Bool
isValidRopePos (H h) (T t) = t == h || t `elem` neighbors h

toTail :: Head -> Tail
toTail (H h) = T h

simulateRope :: [Dir] -> State RopeState EndVal
simulateRope [] = do
  (_, _, v) <- get
  return v
simulateRope (d : ds) = do
  (h, t, v) <- get
  let newH = addDir h d
  if isValidRopePos newH t
    then put (newH, t, v)
    else -- put t as the old h pos
      let newTail = toTail h in put (newH, newTail, Set.insert newTail v)
  simulateRope ds

startT :: Tail
startT = T origin

runSim :: [Dir] -> Visited
runSim dirs = evalState (simulateRope dirs) (H origin, T origin, Set.fromList [startT])

part1 :: IO ()
part1 = do
  print "part1"
  input <- concatMap readCommand <$> readInputLinesMapper id
  print $ Set.size $ runSim input
  return ()

toHead :: Tail -> Head
toHead (T t) = H t

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
