module Utils.Mod where

import Data.List
-- import System.FilePath (combine, takeDirectory)

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Void
import System.Environment (getArgs)
import Text.Megaparsec (MonadParsec (eof), Parsec, parse, setInput)
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf

readInputLines :: IO [String]
readInputLines = lines <$> getRawInput

readInputLinesMapper :: (String -> b) -> IO [b]
readInputLinesMapper f = map f <$> readInputLines

readInputLinesInteger :: IO [Integer]
readInputLinesInteger = map read . lines <$> getRawInput

readInputLinesParser :: Parser a -> IO [a]
readInputLinesParser p = map (parseThrow p) <$> readInputLines

removeEmptyString :: [String] -> [String]
removeEmptyString = filter (not . null)

readNormalInput :: String -> IO String
readNormalInput day = readFile (printf "app/Day%02d/in" (read day :: Integer))

readExampleInput :: String -> IO String
readExampleInput day = readFile (printf "app/Day%02d/in.example" (read day :: Integer))

readDayFile :: String -> String -> IO String
readDayFile day fName = readFile (printf "app/Day%02d/%s" (read day :: Integer) fName)

splitOnBlankInput :: [String] -> [[String]]
splitOnBlankInput = splitOn [""]

-- | Get the input for the given day.
--
-- If a filename is provided in the command line that will be used as the
-- input file.
--
-- If the filename is @-@ the stdin will be used as the input file.
--
-- Otherwise the input text file corresponding to the day number will be used.
getRawInput :: IO String
getRawInput =
  do
    day : _ : xs <- getArgs
    case xs of
      [] -> readNormalInput day
      "0" : _ -> readNormalInput day
      "1" : _ -> readExampleInput day
      "true" : _ -> readExampleInput day
      "-" : _ -> getContents
      fileName : _ -> readDayFile day fileName

parseIntsWithSep :: String -> String -> [Int]
parseIntsWithSep sep str = map read $ splitOn sep str

-- https://github.com/glguy/advent2019/blob/master/common/Advent.hs

type Parser = Parsec Void String

getParsedInput :: FilePath -> Parser a -> IO a
getParsedInput path p =
  do
    input <- readFile path
    case parse p "input" input of
      Left e -> fail (errorBundlePretty e)
      Right a -> return a

readParsedLines :: FilePath -> Parser a -> IO [a]
readParsedLines path p = do
  file <- readFile path
  return (getParsedLines file p)

-- | Run a parser with 'parseLines' on the input file.
getParsedLines :: String -> Parser a -> [a]
getParsedLines input p =
  case parseLines p input of
    Left string -> error string
    Right res -> res

-- | Run a parser on each line of the input file. Each line will be parsed
-- in isolation. The parser must consume the whole line.
--
-- >>> parseLines (Control.Applicative.many anySingle) "12\n34\n"
-- Right ["12","34"]
-- >>> parseLines number "12\n34\n"
-- Right [12,34]
parseLines :: Parser a -> String -> Either String [a]
parseLines p input =
  case parse (traverse parse1 (lines input)) "input" input of
    Left e -> Left (errorBundlePretty e)
    Right a -> Right a
  where
    parse1 x = setInput x *> p <* eof <* setInput "\n" <* newline

parseThrow :: Parser a -> String -> a
parseThrow p input = case res of
  Left e -> error $ errorBundlePretty e
  Right a -> a
  where
    res = parse p "" input

-- | Parse a signed integral number
number :: Integral a => Parser a
number = signed (return ()) decimal

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc + 1 else acc) 0

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a, b) -> a : chunks n b

-- >>> chunkOverlap 3 [1,2,3,4,5,6,7,8,9,10]
-- [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9],[8,9,10]]
chunkOverlap :: Int -> [a] -> [[a]]
chunkOverlap n = takeWhile ((== n) . length) . transpose . take n . iterate tail

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [(x, l ++ r) | (l, x : r) <- zip (inits xs) (tails xs)]

-- | Iterate through list with index
enumerate :: [b] -> [(Int, b)]
enumerate x = zip [0 .. length x - 1] x

-- >>> cartProd [1,2,3] "abc"
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- >>> cartProdSelf [1,2,3]
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
cartProdSelf :: Eq b => [b] -> [(b, b)]
cartProdSelf xs = [(x, y) | x <- xs, y <- xs, x /= y]

uniqPairs :: Ord b => [b] -> [(b, b)]
uniqPairs l = nub [(x, y) | x <- l, y <- l, x < y]

type BinNum = [Int]

toDecimal :: BinNum -> Int
toDecimal bits = foldl (\accSum (idx, val) -> accSum + val * (2 ^ idx)) 0 (zip [0 .. (length bits)] (reverse bits))

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = error "invalid list for tup2"

takeAndDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeAndDropWhile = span

-- use as "_ <- printWithNewLines input"
printWithNewLines :: (Foldable t, Show a) => t a -> IO ()
printWithNewLines = mapM_ print

insertList :: (Foldable t, Ord k) => Map.Map k a -> t (k, a) -> Map.Map k a
insertList = foldl (\m (k, v) -> Map.insert k v m)

isInRange :: Ord a => a -> a -> a -> Bool
isInRange lower upper x = lower <= x && x <= upper

(<?) :: Ord a => a -> (a, a) -> Bool
(<?) = flip (uncurry isInRange)

printNewLine :: (Foldable t, Show a) => t a -> IO ()
printNewLine = mapM_ print
