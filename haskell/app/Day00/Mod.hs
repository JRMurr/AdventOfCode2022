{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day00.Mod where

import Control.Monad.State
import Data.Maybe
import Utils.Mod

isBalanced :: String -> Bool
isBalanced s = isBalanced' s empty

type Pair = (Char, Char)

newtype Stack a = Stack [a]
  deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

bPairs :: [Pair]
bPairs = [('{', '}'), ('[', ']'), ('(', ')')]

getClose :: Char -> Maybe Char
getClose c = listToMaybe [close | (open, close) <- bPairs, open == c]

isBalanced' :: String -> Stack Char -> Bool
isBalanced' [] (Stack s) = null s
isBalanced' (x : xs) s = case getClose x of
  Just close -> isBalanced' xs (push close s)
  Nothing ->
    ( case pop s of
        Just (c, s') -> (c == x) && isBalanced' xs s'
        Nothing -> False
    )

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLines
  print $ map isBalanced input
  return ()

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

type GameValue = Int

type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score
playGame (x : xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c' -> put (not on, score)
    _ -> put (on, score)
  playGame xs

startState :: GameState
startState = (False, 0)

part2 :: IO ()
part2 = do
  print $ evalState (playGame "abcaaacbbcabbab") startState

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
