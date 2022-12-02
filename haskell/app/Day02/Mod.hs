module Day02.Mod where

import Utils.Mod

data RPS = Rock | Paper | Scissors deriving (Read, Show, Ord, Eq)

-- each elms beats the prev and loses to its succ
relation :: [RPS]
relation = [Rock, Paper, Scissors]

-- | Get the rps that beats this
--
-- >>> getWinner Paper
-- Scissors
getWinner :: RPS -> RPS
getWinner r = dropWhile (/= r) (cycle relation) !! 1

-- | Get the rps that loses to this
--
-- >>> getLoser Paper
-- Rock
getLoser :: RPS -> RPS
getLoser r = dropWhile (/= r) ((cycle . reverse) relation) !! 1

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
beats p1 p2 = p2 == getLoser p1

playGame :: Game -> Int
playGame g = (rpsVal . snd) g + playGame' g
  where
    playGame' (p1, p2)
      | p1 == p2 = drawScore
      | beats p2 p1 = winScore
      | otherwise = 0

part1 :: IO ()
part1 = do
  print "part1"
  input <- map readLineP1 <$> readInputLines
  print $ sum $ map playGame input
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
playGameP2 (rps, Win) = playGame (rps, getWinner rps)
playGameP2 (rps, Lose) = playGame (rps, getLoser rps)

part2 :: IO ()
part2 = do
  print "part2"
  input <- map readLineP2 <$> readInputLines
  print $ sum $ map playGameP2 input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
