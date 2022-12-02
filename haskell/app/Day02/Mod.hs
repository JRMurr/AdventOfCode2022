module Day02.Mod where

import Utils.Mod

data RPS = Rock | Paper | Scissors deriving (Read, Show, Ord, Eq)

readRPS :: Char -> RPS
readRPS 'A' = Rock
readRPS 'B' = Paper
readRPS 'C' = Scissors
readRPS 'X' = Rock
readRPS 'Y' = Paper
readRPS 'Z' = Scissors
readRPS _ = error "invalid rps"

type Game = (RPS, RPS)

readLineP1 :: String -> (RPS, RPS)
readLineP1 [p1, ' ', p2] = (readRPS p1, readRPS p2)
readLineP1 _ = error "invalid line"

drawScore :: Int
drawScore = 3

winScore :: Int
winScore = 6

rpsVal :: RPS -> Int
rpsVal Rock = 1
rpsVal Paper = 2
rpsVal Scissors = 3

-- true if the first param beats the second
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Scissors Paper = True
beats Paper Rock = True
beats _ _ = False

playGame :: Game -> Int
playGame g@(_, p2) = rpsVal p2 + playGame' g

playGame' :: Game -> Int
playGame' (p1, p2)
  | p1 == p2 = drawScore
  | beats p2 p1 = winScore
  | otherwise = 0

part1 :: IO ()
part1 = do
  print "part1"
  input <- (map readLineP1) <$> readInputLines
  print $ sum $ map (playGame) input
  return ()

data Outcome = Win | Lose | Draw deriving (Read, Show, Ord, Eq)

readOutcome :: Char -> Outcome
readOutcome 'X' = Lose
readOutcome 'Y' = Draw
readOutcome 'Z' = Win
readOutcome _ = error "invalid out"

type GameP2 = (RPS, Outcome)

readLineP2 :: String -> GameP2
readLineP2 [p1, ' ', p2] = (readRPS p1, readOutcome p2)
readLineP2 _ = error "invalid"

playGameP2 :: GameP2 -> Int
playGameP2 (rps, Draw) = drawScore + rpsVal rps
playGameP2 (Scissors, Win) = winScore + rpsVal Rock
playGameP2 (Rock, Win) = winScore + rpsVal Paper
playGameP2 (Paper, Win) = winScore + rpsVal Scissors
playGameP2 (Scissors, Lose) = rpsVal Paper
playGameP2 (Rock, Lose) = rpsVal Scissors
playGameP2 (Paper, Lose) = rpsVal Rock

part2 :: IO ()
part2 = do
  print "part2"
  input <- (map readLineP2) <$> readInputLines
  print $ sum $ map (playGameP2) input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
