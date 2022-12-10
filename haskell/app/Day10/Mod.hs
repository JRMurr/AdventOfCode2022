module Day10.Mod where

import Data.List.Split (splitOn)
import Data.List.Split.Internals (chunksOf)
import qualified Debug.Trace as Debug
import Utils.Mod

data Command = NoOp | AddX Int deriving (Show)

toCommand :: String -> Command
toCommand "noop" = NoOp
toCommand s = AddX (read num)
  where
    ["addx", num] = splitOn " " s

runCommand :: Command -> Int -> [Int]
runCommand NoOp xVal = [xVal]
runCommand (AddX add) xVal = [xVal, xVal + add]

runProg' :: [Command] -> Int -> [Int]
runProg' [] x = [x]
runProg' (c : cs) xVal = commandRes ++ runProg' cs newX
  where
    commandRes = runCommand c xVal
    newX = last commandRes

runProg :: [Command] -> [Int]
runProg c = 1 : 1 : runProg' c 1

calcStrength :: [Int] -> Int
calcStrength xVals = Debug.trace (show idxVals) $ sum $ [idx * val | (idx, val) <- idxVals]
  where
    points = take 6 $ iterate (40 +) 20
    idxVals = [(idx, xVals !! idx) | idx <- points]

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesMapper toCommand
  let strengths = runProg input
  print $ calcStrength strengths
  return ()

render :: [Int] -> String
render x = unlines $ map renderRow rows
  where
    rows = chunksOf 40 $ tail x
    renderRow = zipWith (\i x -> if abs (i - x) < 2 then '#' else ' ') [0 ..]

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper toCommand
  let strengths = runProg input
  putStrLn $ render strengths
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
