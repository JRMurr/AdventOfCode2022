module Day05.Mod where

import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Utils.Mod

newtype Stack a = Stack [a]
  deriving (Show)

stackFromLst :: [a] -> Stack a
stackFromLst = Stack

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

type CargoStack = Stack Char

type Stacks = Map Int CargoStack

data Move = Move {amount :: Int, from :: Int, to :: Int} deriving (Show)

-- each crate will be at 1 + (idx *4) if idx has one
-- drop first col so its cleaner (just idx *4)
readStackLine :: String -> [Maybe Char]
readStackLine (_ : line) = [if x /= ' ' then Just x else Nothing | (idx, x) <- enumerate line, (idx `mod` 4) == 0]
readStackLine l = error ("invalid line" ++ show l)

-- Instead of reading top down, we will read from the bottom up so we can just push each item on
readStacks :: [String] -> Map Int [Char] -> Stacks
readStacks [] curr = Map.map stackFromLst curr
readStacks (line : xs) curr = readStacks xs newCurr
  where
    possibleCrates = enumerate $ readStackLine line
    reducer stack (_, Nothing) = stack
    reducer stack (idx, Just x) = Map.insertWith (++) (idx + 1) [x] stack -- add idx +1 so key is the same as moves
    newCurr = foldl reducer curr possibleCrates

readMove :: String -> Move
readMove s = Move {amount = read amnt, from = read fromAmt, to = read toAmt}
  where
    ["move", amnt, "from", fromAmt, "to", toAmt] = splitOn " " s

parseInput :: [String] -> (Stacks, [Move])
parseInput input = (parsedStacks, parsedMoves)
  where
    [stacks, moves] = splitOnBlankInput input
    parsedStacks = readStacks (drop 1 $ reverse stacks) Map.empty -- go bottom up for push and drop the index row
    parsedMoves = map readMove moves

popIdx :: Int -> Stacks -> (Char, Stacks)
popIdx idx stacks = (popped, Map.insert idx newStack stacks)
  where
    fromStack = stacks ! idx
    Just (popped, newStack) = pop fromStack

pushIdx :: Int -> Char -> Stacks -> Stacks
pushIdx idx new stacks = Map.insert idx newStack stacks
  where
    fromStack = stacks ! idx
    newStack = push new fromStack

handleMove :: Move -> Stacks -> Stacks
handleMove Move {amount = 0} stacks = stacks
handleMove Move {amount = amnt, from = fIdx, to = toIdx} stacks = handleMove Move {amount = amnt - 1, from = fIdx, to = toIdx} stackFinal
  where
    (popped, stackPop) = popIdx fIdx stacks
    stackFinal = pushIdx toIdx popped stackPop

handleMoves :: [Move] -> Stacks -> Stacks
handleMoves m s = foldl (flip handleMove) s m

getTop :: Stacks -> String
getTop s = [getTopChar (s ! idx) | idx <- Map.keys s]
  where
    getTopChar stack = fromMaybe '.' (peek stack)

part1 :: IO ()
part1 = do
  print "part1"
  (stack, moves) <- parseInput <$> readInputLines
  print (stack, moves)
  let appliedMoves = handleMoves moves stack
  print $ getTop appliedMoves
  return ()

popIdxAmnt :: Int -> Int -> Stacks -> ([Char], Stacks)
popIdxAmnt idx amnt stacks = (popped, Map.insert idx (Stack remaing) stacks)
  where
    (Stack fromStack) = stacks ! idx
    (popped, remaing) = splitAt amnt fromStack

pushLst :: Int -> [Char] -> Stacks -> Stacks
pushLst idx lst stacks = Map.insert idx (Stack newStack) stacks
  where
    (Stack fromStack) = stacks ! idx
    newStack = lst ++ fromStack

handleMoveP2 :: Move -> Stacks -> Stacks
handleMoveP2 Move {amount = amnt, from = fIdx, to = toIdx} stacks = stackFinal
  where
    (popped, stackPop) = popIdxAmnt fIdx amnt stacks
    stackFinal = pushLst toIdx popped stackPop

part2 :: IO ()
part2 = do
  print "part2"
  (stack, moves) <- parseInput <$> readInputLines
  print (stack, moves)
  let appliedMoves = foldl (flip handleMoveP2) stack moves
  print $ getTop appliedMoves
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
