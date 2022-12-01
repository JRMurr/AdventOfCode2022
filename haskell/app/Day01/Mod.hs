module Day01.Mod where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Utils.Mod

parseInput :: [String] -> [[Int]]
parseInput s = map (map read) (splitOn [""] s)

part1 :: IO ()
part1 = do
  input <- parseInput <$> readInputLines
  let maxElf = maximum $ map sum input
  print maxElf
  return ()

part2 :: IO ()
part2 = do
  input <- parseInput <$> readInputLines
  let sorted = sortOn negate $ map sum input
  let top = take 3 sorted
  print $ sum top
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
