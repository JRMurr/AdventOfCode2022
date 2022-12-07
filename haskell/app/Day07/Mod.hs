{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Day07.Mod where

import Control.Monad.State
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import Utils.MapUtils (mapEntry)
import Utils.Mod

data File = File {fName :: String, fSize :: Int} deriving (Show)

type Folder = Map String FolderEntry

-- -- A folder entry can be a dir with a name and children or a file
data FolderEntry = Dir Folder | F File deriving (Show)

-- data CdCommand = ToDir String | UpDir deriving (Show)

-- type Folder = [FolderEntry]

-- type FolderPath = [String]

parseLsOutLine :: Folder -> String -> Folder
parseLsOutLine currFolder s = Map.insert name value currFolder
  where
    (name, value) = case splitOn " " s of
      ["dir", folderName] -> (folderName, Dir Map.empty)
      [size, fileName] -> (fileName, F (File {fName = fileName, fSize = read size}))
      _ -> error ("bad ls" ++ s)

parseLsLines :: Folder -> [String] -> Folder
parseLsLines = foldl parseLsOutLine

-- getFilePath :: [String] -> Folder -> FolderEntry
-- getFilePath [] _ = error "empty path"
-- getFilePath (x : xs) f = case Map.lookup x f of
--   Just (Dir subF) -> getFilePath xs subF
--   Just file@(F _) -> if null xs then file else error ("invalid path lookup: " ++ show (x : xs) ++ ", found file:" ++ show xs)
--   Nothing -> error ("Invalid path: " ++ show (x : xs) ++ " got nothing")

getFilePath :: [String] -> FolderEntry -> FolderEntry
getFilePath [] f = f
getFilePath (x : xs) f@(F _) = error $ "invalid path:" ++ show (x : xs) ++ ", at file: " ++ show f
getFilePath (x : xs) (Dir folder) =
  getFilePath
    xs
    ( case Map.lookup x folder of
        Nothing -> error ("path invalid: " ++ show (x : xs))
        Just fe -> fe
    )

setFilePath :: [String] -> FolderEntry -> Folder -> Folder
setFilePath [] _ _ = error "empty path"
setFilePath [k] v m = Map.insert k v m
setFilePath (x : xs) v m =
  mapEntry
    x
    ( \case
        F _ -> error "file found not folder"
        Dir folder -> Dir (setFilePath xs v folder)
    )
    m

type ParsingState = ([String], Folder)

type ParsingRes = Folder

getNewFilePath :: String -> [String] -> [String]
getNewFilePath cdCommand currPath = case path of
  ".." -> init currPath
  _ -> currPath ++ [path]
  where
    ["$", "cd", path] = splitOn " " cdCommand

parseInput :: [String] -> State ParsingState ParsingRes
parseInput [] = do
  (_, res) <- get
  return res
parseInput ("$ ls" : xs) = do
  (filePath, folder) <- get
  let Dir currFolder = getFilePath filePath (Dir folder)
  let (lsLines, rest) = span (\x -> head x /= '$') xs
  let newCurr = Dir (parseLsLines currFolder lsLines)
  let newFolder = setFilePath filePath newCurr folder
  put (filePath, newFolder)
  parseInput rest
parseInput (cdCommand : xs) = do
  (filePath, folder) <- get
  let newPath = getNewFilePath cdCommand filePath
  put (newPath, folder)
  parseInput xs

handleInput :: [String] -> Folder
handleInput s = evalState (parseInput s) ([], Map.fromList [("/", Dir Map.empty)])

type FolderSize = (String, Int)

-- getFolderSizes :: Folder -> String -> State [FolderSize] [FolderSize]
-- getFolderSizes f name = do
--   sizes <- get
--   let
--   return []

part1 :: IO ()
part1 = do
  print "part1"
  input <- handleInput <$> readInputLinesMapper id
  print input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
