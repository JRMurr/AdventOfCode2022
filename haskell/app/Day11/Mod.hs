module Day11.Mod where

import Data.Char (isNumber)
import Data.List (sortBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils.Mod

data OpVal = Old | Val Int deriving (Show)

data Op = Mul OpVal | Add OpVal deriving (Show)

newtype MonkeyId = ID Int deriving (Show, Eq, Ord)

data Monkey = M {mId :: MonkeyId, mItems :: [Int], mOp :: Op, mDivTest :: Int, tMon :: MonkeyId, fMon :: MonkeyId, inspectCount :: Int} deriving (Show)

parseItems :: String -> [Int]
parseItems s = map read $ words numStrs
  where
    numStrs = filter (/= ',') $ dropWhile (not . isNumber) s

parseOp :: String -> Op
parseOp s =
  let ["Operation:", "new", "=", "old", operationStr, num] = words s
   in case operationStr of
        "*" -> Mul (parseOpVal num)
        "+" -> Add (parseOpVal num)
        _ -> error $ "invalid op: " ++ s
  where
    parseOpVal "old" = Old
    parseOpVal num = Val (read num)

getEndingNum :: String -> Int
getEndingNum s = read $ dropWhile (not . isNumber) s

parseMonkey :: [String] -> Monkey
parseMonkey [mIdStr, items, op, test, trueVal, fVal] =
  M
    { mId = ID $ getEndingNum (init mIdStr), -- drop : at the end
      mItems = parseItems items,
      mOp = parseOp op,
      mDivTest = getEndingNum test,
      tMon = ID $ getEndingNum trueVal,
      fMon = ID $ getEndingNum fVal,
      inspectCount = 0
    }
parseMonkey _ = error "invalid monkey"

type MonkeyMap = Map MonkeyId Monkey

parseMonkeyMap :: [String] -> MonkeyMap
parseMonkeyMap = Map.fromList . map ((\m -> (mId m, m)) . parseMonkey) . splitOnBlankInput

evalOp :: Op -> Int -> Int
evalOp (Mul Old) x = x * x
evalOp (Add Old) x = x + x
evalOp (Mul (Val mul)) x = x * mul
evalOp (Add (Val add)) x = x + add

-- | Worry update function. Divides by 3 or does a modulus.
type DivFunc = Int -> Int

handleItem :: DivFunc -> Monkey -> Int -> (MonkeyId, Int)
handleItem fn monkey@(M {mOp = op, mDivTest = divTest}) item = (newMonkey, relaxedLevel)
  where
    opLevel = evalOp op item
    relaxedLevel = fn opLevel
    isDiv = relaxedLevel `mod` divTest == 0
    newMonkey = if isDiv then tMon monkey else fMon monkey

handleItems :: DivFunc -> Monkey -> [(MonkeyId, Int)]
handleItems f monkey = map (handleItem f monkey) (mItems monkey)

addToMonkeyItems :: MonkeyMap -> (MonkeyId, Int) -> MonkeyMap
addToMonkeyItems mMap (monkeyId, newItem) = Map.insert monkeyId (currMonkey {mItems = newItems}) mMap
  where
    currMonkey = mMap ! monkeyId
    currItems = mItems currMonkey
    newItems = currItems ++ [newItem]

runMonkeyTurn :: DivFunc -> MonkeyMap -> MonkeyId -> MonkeyMap
runMonkeyTurn f mMap monkeyId = foldl addToMonkeyItems updatedCurrMap handledItems
  where
    currMonkey = mMap ! monkeyId
    handledItems = handleItems f currMonkey
    numInspect = inspectCount currMonkey + length handledItems
    updatedCurrMap = Map.insert monkeyId (currMonkey {mItems = [], inspectCount = numInspect}) mMap

runRound :: DivFunc -> MonkeyMap -> MonkeyMap
runRound f mMap = foldl (runMonkeyTurn f) mMap monkeyIds
  where
    monkeyIds = Map.keys mMap -- should be asc

runXRounds :: DivFunc -> Int -> MonkeyMap -> MonkeyMap
-- runXRounds 0 mMap = mMap
-- runXRounds x mMap = let newMap = runRound mMap in runXRounds (x - 1) newMap
runXRounds f x mMap = last $ take (x + 1) $ iterate (runRound f) mMap

getTopMonkeyCounts :: MonkeyMap -> [Int]
getTopMonkeyCounts res = sortBy (flip compare) $ map inspectCount $ Map.elems res

part1 :: IO ()
part1 = do
  print "part1"
  input <- parseMonkeyMap <$> readInputLines
  let res = runXRounds (`div` 3) 20 input
  let counts = getTopMonkeyCounts res
  print counts
  print $ product $ take 2 counts
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- parseMonkeyMap <$> readInputLines
  -- I wish i figured this out on my on...
  -- TLDR: we don't care how big the number is just if its divisible by the number we want
  -- We can use the Chinese Remainder Theorem since all divisors are relatively prime
  -- so we divide each number by the prod of the divisors to keep the numbers small
  let divs = product $ Map.map mDivTest input
  let res = runXRounds (`mod` divs) 10000 input
  let counts = getTopMonkeyCounts res
  print counts
  print $ product $ take 2 counts
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
