module Day19.Mod where

import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod
import Utils.ParseUtils (parseInt)

parseSingleOreRobot :: String -> Parser Int
parseSingleOreRobot robotType = label ("robotType: " ++ robotType) $ do
  _ <- string ("Each " ++ robotType ++ " robot costs ")
  amt <- parseInt
  _ <- string " ore."
  return amt

parseDoubleOreRobot :: (String, String) -> Parser (Int, Int)
parseDoubleOreRobot (robotType, specialOre) = label ("robotType: " ++ robotType) $ do
  _ <- string ("Each " ++ robotType ++ " robot costs ")
  amt <- parseInt
  _ <- string " ore and "
  specialAmt <- parseInt
  _ <- string (" " ++ specialOre ++ ".")
  return (amt, specialAmt)

parseBlueprintId :: Parser Int
parseBlueprintId = label "blueprint id" $ do
  _ <- string "Blueprint "
  bluePrintId <- parseInt
  _ <- char ':'
  pure bluePrintId

data Resource = Ore | Clay | Obsidian | Geode deriving (Show, Eq)

type Requirment = (Resource, Int)

data Blueprint = BP
  { bId :: Int,
    oreReq :: Requirment,
    clayReq :: Requirment,
    obsidianReq :: [Requirment],
    geodeReq :: [Requirment]
  }
  deriving (Show, Eq)

parseBlueprint :: Parser Blueprint
parseBlueprint = label "blueprint" $ do
  bluePrintId <- parseBlueprintId
  _ <- space
  ore <- parseSingleOreRobot "ore"
  _ <- space
  clay <- parseSingleOreRobot "clay"
  _ <- space
  (obsOre, obsClay) <- parseDoubleOreRobot ("obsidian", "clay")
  _ <- space
  (geoOre, geoObs) <- parseDoubleOreRobot ("geode", "obsidian")
  return
    ( BP
        { bId = bluePrintId,
          oreReq = (Ore, ore),
          clayReq = (Clay, clay),
          obsidianReq = [(Ore, obsOre), (Clay, obsClay)],
          geodeReq = [(Ore, geoOre), (Obsidian, geoObs)]
        }
    )

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesParser parseBlueprint
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
