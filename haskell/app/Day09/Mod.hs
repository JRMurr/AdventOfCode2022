{-# LANGUAGE InstanceSigs #-}

module Day09.Mod where

import Control.Monad.State
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Coords
import Utils.Mod

newtype Head = H Coord deriving (Show, Eq, Ord)

newtype Tail = T Coord deriving (Show, Eq, Ord)

type Visited = Set Tail

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

type EndVal = Visited

isValidRopePos :: Head -> Tail -> Bool
isValidRopePos (H h) (T t) = t == h || t `elem` neighbors h

startT :: Tail
startT = T origin

toHead :: Tail -> Head
toHead (T t) = H t

-- The head, 8 tails, and the places nine has visited
type LongRope = (Head, [Tail], Visited)

towardsH :: Head -> Tail -> Dir
towardsH (H h) (T t) = getDirTowards h t

longRopeReducer :: (Head, [Tail]) -> Tail -> (Head, [Tail])
longRopeReducer (newPos, acc) t =
  if isValidRopePos newPos t
    then (toHead t, t : acc)
    else let newTail = (addDir t $ towardsH newPos t) in (toHead newTail, newTail : acc)

longRopeMove :: Dir -> Head -> [Tail] -> (Head, [Tail])
longRopeMove d h tails = (newH, reverse newT)
  where
    newH = addDir h d
    (_, newT) = foldl longRopeReducer (newH, []) tails

simulateLongRope :: [Dir] -> State LongRope EndVal
simulateLongRope [] = do
  (_, _, v) <- get
  return v
simulateLongRope (d : ds) = do
  (h, t, v) <- get
  let (newH, newT) = longRopeMove d h t
  put (newH, newT, Set.insert (last newT) v)
  simulateLongRope ds

runLongSim :: [Dir] -> Visited
runLongSim dirs = evalState (simulateLongRope dirs) (H origin, replicate 9 startT, Set.fromList [startT])

runSim :: [Dir] -> Visited
runSim dirs = evalState (simulateLongRope dirs) (H origin, [startT], Set.fromList [startT])

part1 :: IO ()
part1 = do
  print "part1"
  input <- concatMap readCommand <$> readInputLinesMapper id
  print $ Set.size $ runSim input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- concatMap readCommand <$> readInputLinesMapper id
  let seen = runLongSim input
  -- printf $ drawCoordSet (Set.map (\(T c) -> c) seen)
  print $ Set.size $ Set.map (\(T c) -> c) seen
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
