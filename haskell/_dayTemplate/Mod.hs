module DayXX.Mod where

import Utils.Mod

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLines
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLines
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]