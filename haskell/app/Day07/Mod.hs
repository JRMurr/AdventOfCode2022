{-# OPTIONS_GHC -Wall #-}

module Day07.Mod where

import Control.Monad.State
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Utils.Mod
import Prelude hiding (FilePath)
import Data.Maybe
import Data.List (isPrefixOf, tails)
import qualified Data.Set as Set


type FilePath = [String]

type Files = Map FilePath Int

parseFile :: String -> Maybe (Int, String)
parseFile s = case splitOn " " s of
  ["dir", _] -> Nothing
  [size, fileName] -> Just (read size, fileName)
  _ -> Nothing

getFilesFromLs :: [String] -> [(Int, String)]
getFilesFromLs = mapMaybe parseFile


parseInput' :: [String] -> FilePath -> Files -> Files
parseInput' [] _ f = f
parseInput' ("$ ls" : xs) filePath f = parseInput' rest filePath  newFMap
  where
    (lsLines, rest) = span (\x -> head x /= '$') xs
    files = getFilesFromLs lsLines
    newFMap = foldl (\m (size, _) -> Map.insertWith (+) filePath size m) f files
parseInput' ("$ cd .." : xs) filePath f = parseInput' xs (init filePath) f
parseInput' ("$ cd /" : xs) _ f = parseInput' xs ["/"] (Map.insertWith (+) ["/"] 0 f)
parseInput' (cdCommand : xs) filePath f = parseInput' xs (filePath ++ [path]) (Map.insertWith (+) (filePath ++ [path]) 0 f)
 where
  ["$", "cd", path] = splitOn " " cdCommand


parseInput :: [String] -> Files
parseInput input = parseInput' input [] Map.empty

type FolderSize = (FilePath, Int)


getFolderSizes :: Files -> [FolderSize]
getFolderSizes files = map (\p -> (p, getFolderSize p files)) filePaths
 where
  filePaths = Map.keys files

getFolderSize :: FilePath -> Files -> Int
getFolderSize fp files = sum $ map (files Map.!) validFiles
 where
  filePaths = Map.keys files
  validFiles = filter (fp `isPrefixOf`) filePaths

getDirs :: Files -> [FilePath]
getDirs files = Set.elems $ Set.fromList rawDirs
 where
  filePaths = Map.keys files
  rawDirs = concatMap (tail . tails. init) (filter (\x -> length x > 1) filePaths)



part1 :: IO ()
part1 = do
  print "part1"
  input <- parseInput <$> readInputLinesMapper id
  let sizes = getFolderSizes input
  print $ sum $ [s | (_, s) <- sizes, s <= 100000]
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- parseInput <$> readInputLinesMapper id
  let sizes = getFolderSizes input
  let sizeMap = Map.fromList sizes
  let rootSize = sizeMap ! ["/"]
  let neededSpace = 30000000 - (70000000 - rootSize)
  print $ minimum [s | (_, s) <- sizes, s >= neededSpace]
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
