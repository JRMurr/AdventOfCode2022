module Day04.Mod where

import Utils.Mod

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesMapper id
  print input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
