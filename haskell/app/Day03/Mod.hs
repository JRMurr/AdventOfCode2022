module Day03.Mod where

import Data.Char (isLower, ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils.Mod

type Compartment = Set Char

type RuckSack = (Compartment, Compartment)

readCompartment :: String -> Compartment
readCompartment = Set.fromList

readRuckSack :: String -> RuckSack
readRuckSack s = (readCompartment first, readCompartment second)
  where
    halfIdx = length s `div` 2
    (first, second) = splitAt halfIdx s

findCommon :: RuckSack -> Char
findCommon (x, y) = head $ Set.elems $ Set.intersection x y

charPriority :: Char -> Int
charPriority x
  | isLower x = ord x - 96
  | otherwise = (ord x - 65) + 27

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesMapper (charPriority . findCommon . readRuckSack)
  print (sum input)
  return ()

-- no splitting in half just 3 lines
type RuckSackP2 = (Compartment, Compartment, Compartment)

readRuckP2 :: [String] -> RuckSackP2
readRuckP2 [x, y, z] = (readCompartment x, readCompartment y, readCompartment z)
readRuckP2 _ = error "bad chunk"

findCommonP2 :: RuckSackP2 -> Char
findCommonP2 (x, y, z) = head $ Set.elems $ Set.intersection z $ Set.intersection x y

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper ((charPriority . findCommonP2 . readRuckP2) . chunks 3)
  print (sum input)
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
