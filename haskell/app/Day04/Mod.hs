module Day04.Mod where

import Data.List.Split (splitOn)
import Utils.Mod

type Assignment = (Int, Int)

readAssignment :: String -> Assignment
readAssignment s = tuplify2 $ parseIntsWithSep "-" s

readLine :: String -> (Assignment, Assignment)
readLine s = tuplify2 (map readAssignment $ splitOn "," s)

fullyContains :: Assignment -> Assignment -> Bool
fullyContains (x1, y1) (x2, y2)
  | x1 <= x2 && y1 >= y2 = True
  | x2 <= x1 && y2 >= y1 = True
  | otherwise = False

part1 :: IO ()
part1 = do
  print "part1"
  input <- count (uncurry fullyContains) <$> readInputLinesMapper readLine
  print input
  return ()

inAssignment :: Int -> Assignment -> Bool
inAssignment a (x, y) = a >= x && a <= y

overlaps :: Assignment -> Assignment -> Bool
overlaps a1@(x1, y1) a2@(x2, y2)
  | inAssignment x1 a2 || inAssignment y1 a2 = True
  | inAssignment x2 a1 || inAssignment y2 a1 = True
  | otherwise = False

part2 :: IO ()
part2 = do
  print "part2"
  input <- count (uncurry overlaps) <$> readInputLinesMapper readLine
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
