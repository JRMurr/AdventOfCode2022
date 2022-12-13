{-# LANGUAGE InstanceSigs #-}

module Day13.Mod where

import Data.List (intercalate, sort)
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod

data Packet = L [Packet] | I Int deriving (Eq)

instance Show Packet where
  show (I v) = show v
  show (L vals) = "[" ++ intercalate "," (map show vals) ++ "]"

-- parsePacket :: String -> Packet
-- parsePacket
-- https://serokell.io/blog/parser-combinators-in-haskell#megaparsec-tutorial
parseInt :: Parser Int
parseInt = read <$> some numberChar <?> "integer"

parsePList :: Parser [Packet]
parsePList = between (char '[') (char ']') (sepBy parsePacket (char ','))

parsePacket :: Parser Packet
parsePacket =
  choice
    [ I <$> parseInt,
      L <$> parsePList
    ]

readPacketInput :: [String] -> [(Packet, Packet)]
readPacketInput = map (tuplify2 . map (parseThrow parsePacket)) . splitOnBlankInput

-- Get the first just value of a list (if it exists) otherwise Nothing
findFirstJust :: [Maybe a] -> Maybe a
findFirstJust ls = case removeNothings of
  h : _ -> h
  _ -> Nothing
  where
    removeNothings = dropWhile isNothing ls

-- compare two values, if equal return nothing otherwise true if left < right
compareAsMaybe :: Int -> Int -> Maybe Bool
compareAsMaybe left right
  | left == right = Nothing -- keep consuming to see
  | left < right = Just True
  | otherwise = Just False

isOrdered :: (Packet, Packet) -> Maybe Bool
-- both ints
isOrdered (I left, I right) = compareAsMaybe left right
-- both lists
isOrdered (L left, L right) =
  if isJust combinedRes
    then combinedRes
    else compareAsMaybe (length left) (length right)
  where
    combinedRes = findFirstJust $ zipWith (curry isOrdered) left right
-- one is list other is int
isOrdered (left@(L _), right@(I _)) = isOrdered (left, L [right])
isOrdered (left@(I _), right@(L _)) = isOrdered (L [left], right)

part1 :: IO ()
part1 = do
  print "part1"
  input <- readPacketInput <$> readInputLines
  let res = [idx + 1 | (idx, pair) <- enumerate input, isOrdered pair == Just True]
  print $ sum res
  return ()

dividerPackets :: [Packet]
dividerPackets = map (parseThrow parsePacket) ["[[2]]", "[[6]]"]

readPacketInputP2 :: [String] -> [Packet]
readPacketInputP2 input = dividerPackets ++ concatMap (map (parseThrow parsePacket)) (splitOnBlankInput input)

instance Ord Packet where
  (<=) :: Packet -> Packet -> Bool
  -- default nothing to true since if it was nothing they were equal
  (<=) l r = fromMaybe True (isOrdered (l, r))

isDivPacket :: Packet -> Bool
isDivPacket p = p `elem` dividerPackets

part2 :: IO ()
part2 = do
  print "part2"
  input <- sort . readPacketInputP2 <$> readInputLines
  let key = product [idx + 1 | (idx, p) <- enumerate input, isDivPacket p]
  _ <- printWithNewLines input
  print key
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
