module Day19.Mod where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.List (foldl', insert)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.PQueue.Prio.Max (MaxPQueue)
import qualified Data.PQueue.Prio.Max as PQ
import qualified Debug.Trace as Debug
import GHC.OldList (sortBy)
import Text.Megaparsec hiding (State)
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

data Resource = Ore | Clay | Obsidian | Geode deriving (Show, Eq, Ord)

type Requirment = (Resource, Int)

type RobotReqs = Map Resource [Requirment]

data Blueprint = BP
  { bId :: Int,
    reqs :: RobotReqs
  }
  deriving (Show, Eq)

type Robots = [Resource]

type ResourceCounts = Map Resource Int

type Inventory = (Robots, ResourceCounts)

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
          reqs =
            Map.fromList
              [ (Ore, [(Ore, ore)]),
                (Clay, [(Ore, clay)]),
                (Obsidian, [(Ore, obsOre), (Clay, obsClay)]),
                (Geode, [(Ore, geoOre), (Obsidian, geoObs)])
              ]
        }
    )

type Node = (Inventory, Int) -- (Inventory, step)

getDefResourceCounts :: (Ord k, Num a) => Map k a -> k -> a
getDefResourceCounts m r = Map.findWithDefault 0 r m

numSteps :: Int
numSteps = 24

-- for a given amount of time remaining whats the ideal number of geodes you could collect
-- ie each step make a new bot and use the existing to get geodes
idealNumGeodesPerStep :: [Int]
idealNumGeodesPerStep = [((t - 1) * t) `div` 2 | t <- [0 .. 24]]

-- try to build robots in this order
desiredOrder :: [Resource]
desiredOrder = [Geode, Obsidian, Clay, Ore]

-- weight the better robots and resource more
-- does not need to be perfect just try to look at some of the geode nodes faster
-- scoreNode :: Node -> Int
-- scoreNode ((robots, resourceCounts), step) = robotScore + rcScore
--   where
--     resourceScore Geode = 40000
--     resourceScore Obsidian = 20000
--     resourceScore Clay = 5000
--     resourceScore Ore = 0
--     robotScore = sum $ map ((100 *) . resourceScore) robots
--     rcScore = sum $ map (\(r, c) -> resourceScore r * c) (Map.toList resourceCounts)

maxBlueprint :: Blueprint -> Int
maxBlueprint bp = walk [(([Ore], Map.empty), 0)] 0
  where
    walk :: [Node] -> Int -> Int
    walk nStack currMax
      | null nStack = Debug.trace ("fin: " ++ show (bId bp)) $ currMax -- end of queue return our current max
      | step >= numSteps = walk nStack' currMax -- skip
      | idealRemainingGeodes <= currMax = walk nStack' currMax -- not possible to beat the max so skip
      | otherwise = Debug.traceShow (currMax, node) $ walk nStackSorted (max newGeodeCount currMax)
      where
        t = numSteps - step
        idealRemainingGeodes = currGeodes + (numGeodeBots * t) + (idealNumGeodesPerStep !! t)
        node@((robots, resourceCounts), step) = head nStack
        getResourceCount = getDefResourceCounts resourceCounts
        nStack' = tail nStack

        currGeodes = getResourceCount Geode
        numGeodeBots = Utils.Mod.count (== Geode) robots

        resourceCounts' = foldl' (\rc r -> Map.insertWith (+) r 1 rc) resourceCounts robots

        canBuild desiredR = if hasAll then Just requiredResources else Nothing
          where
            requiredResources = reqs bp ! desiredR
            hasAll = all (\(requiredR, c) -> getResourceCount requiredR >= c) requiredResources

        removeCost :: [Requirment] -> ResourceCounts
        removeCost = foldl' (\rc (r, c) -> Map.insertWith (flip (-)) r c rc) resourceCounts'

        getNodeForBuild r cost = ((r : robots, removeCost cost), step + 1)

        -- we always run the robots but we will only try to build 1 robot each step
        successors =
          mapMaybe
            (\r -> getNodeForBuild r <$> canBuild r)
            desiredOrder
            ++ [((robots, resourceCounts'), step + 1)]

        -- nStack'' = foldl' (\q n -> PQ.insert (scoreNode n) n q) nStack' successors
        nStack'' = successors ++ nStack'

        nStackSorted = sortBy (\((r1, _), _) ((r2, _), _) -> r2 `compare` r1) nStack''

        newGeodeCount = getDefResourceCounts resourceCounts' Geode

-- https://stackoverflow.com/a/28550009
maxBlueprintM :: Blueprint -> Int
maxBlueprintM bp = evalState (dfs' (([Ore], Map.empty), 0)) 0
  where
    succ node@((robots, resourceCounts), step)
      | step >= numSteps = []
      | otherwise = Debug.traceShow (node) $ successors
      where
        getResourceCount = getDefResourceCounts resourceCounts

        resourceCounts' = foldl' (\rc r -> Map.insertWith (+) r 1 rc) resourceCounts robots

        canBuild desiredR = if hasAll then Just requiredResources else Nothing
          where
            requiredResources = reqs bp ! desiredR
            hasAll = all (\(requiredR, c) -> getResourceCount requiredR >= c) requiredResources

        removeCost :: [Requirment] -> ResourceCounts
        removeCost = foldl' (\rc (r, c) -> Map.insertWith (flip (-)) r c rc) resourceCounts'

        getNodeForBuild r cost = ((r : robots, removeCost cost), step + 1)

        -- we always run the robots but we will only try to build 1 robot each step
        successors =
          mapMaybe
            (\r -> getNodeForBuild r <$> canBuild r)
            desiredOrder
            ++ [((robots, resourceCounts'), step + 1)]

    couldBeatMax ((robots, resourceCounts), step) currMax = idealRemainingGeodes > currMax
      where
        t = numSteps - step
        getResourceCount = getDefResourceCounts resourceCounts
        currGeodes = getResourceCount Geode
        numGeodeBots = Utils.Mod.count (== Geode) robots
        idealRemainingGeodes = currGeodes + (numGeodeBots * t) + (idealNumGeodesPerStep !! t)

    dfs' :: Node -> State Int Int
    dfs' node =
      (\lst -> if null lst then 0 else maximum lst)
        <$> forM
          (succ node)
          ( \s@((_, rc), _) ->
              get >>= \newMax ->
                if couldBeatMax s newMax
                  then
                    let newGeodeCount = getDefResourceCounts rc Geode
                     in put (max newMax newGeodeCount) >> dfs' s
                  else pure newMax
          )

part1 :: IO ()
part1 = do
  print "part1"
  input <- readInputLinesParser parseBlueprint
  -- let bp = head input
  -- print $ maxBlueprintM bp
  -- print bp
  print $ map maxBlueprintM (tail input)
  -- print $ sum $ map (\bp -> maxBlueprint bp * bId bp) input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
