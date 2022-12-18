module Day16.Mod where

import Data.Function.Memoize (memoize3)
import Data.List (delete)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod

parseInt :: Parser Int
parseInt = label "integer" $ read <$> (some numberChar <|> ((:) <$> char '-' <*> some numberChar))

parseVName :: Parser VName
parseVName = label "vName" $ many letterChar

parseValve :: Parser Valve
parseValve = label "valve" $ do
  _ <- string "Valve "
  name <- parseVName
  _ <- string " has flow rate="
  rate <- parseInt
  _ <- string "; "
  _ <- string "tunnels lead to valves " <|> string "tunnel leads to valve "
  tunnels <- sepBy parseVName (string ", ")
  pure (V {vName = name, vRate = rate, vTunnels = tunnels})

-- newtype VName = Vname String deriving (Show, Ord, Eq)
type VName = String

data Valve = V {vName :: VName, vRate :: Int, vTunnels :: [VName]} deriving (Show)

type ValveMap = Map VName Valve

type Path = (VName, VName) -- (start,end)

type PathLengths = Map Path Int

toVMap :: [Valve] -> ValveMap
toVMap valves = Map.fromList [(vName v, v) | v <- valves]

-- https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm

getPathLength :: PathLengths -> Path -> Int
getPathLength currMap path = Map.findWithDefault 1000000 path currMap -- max bound is sad as a deafult for some reason

calcPathLengths :: ValveMap -> PathLengths
calcPathLengths vMap = foldl computeTriplet initMap vertexTriplets
  where
    valves = Map.keys vMap
    calcValve v = ((vName v, vName v), 0) : [((vName v, t), 1) | t <- vTunnels v]
    initMap = Map.fromList (concatMap calcValve (Map.elems vMap))
    vertexTriplets = [(k, i, j) | k <- valves, i <- valves, j <- valves]
    computeTriplet pMap (k, i, j) = if distIJ > possibleDist then Map.insert (i, j) possibleDist pMap else pMap
      where
        dist x y = getPathLength pMap (x, y)
        distIJ = dist i j
        distIK = dist i k
        distKJ = dist k j
        possibleDist = distIK + distKJ

type Visited = [VName]

type Remaining = Set VName

getMaxScore :: ValveMap -> PathLengths -> Int -> VName -> [VName] -> Int
getMaxScore vMap pMap step curr remaining
  | step < 0 = error $ "too many steps: " ++ show step
  | step == 0 = 0
  | otherwise = maxChild
  where
    checkNeighbor v =
      if dist >= step
        then 0
        else (numStepsOpen * neighborRate) + subScore
      where
        neighborRate = vRate $ vMap ! v
        dist = pMap ! (curr, v)
        numStepsOpen = step - dist - 1
        subScore = getMaxScore vMap pMap numStepsOpen v (delete v remaining)
    scores = map checkNeighbor remaining
    maxChild =
      if (not . null) remaining
        then maximum scores
        else 0

type MemHelper = Int -> VName -> [VName] -> Int

getMaxScoreMem :: ValveMap -> PathLengths -> MemHelper
getMaxScoreMem vMap pMap = memoize3 (getMaxScore vMap pMap)

start :: VName
start = "AA"

runGetMax :: ValveMap -> Int
runGetMax vMap = getMaxScore filtedVMap pMap 30 start (Map.keys filtedVMap)
  where
    pMap = calcPathLengths vMap
    filtedVMap = Map.filter (\v -> vRate v /= 0) vMap

getMaxP2 :: ValveMap -> PathLengths -> MemHelper -> Int -> VName -> Remaining -> Int
getMaxP2 vMap pMap memHelper step curr remaining
  | step < 0 = error $ "too many steps: " ++ show step
  | step == 0 = 0
  | otherwise = maxChild
  where
    checkNeighbor v =
      if dist >= step
        then 0
        else (numStepsOpen * neighborRate) + subScore
      where
        neighborRate = vRate $ vMap ! v
        dist = pMap ! (curr, v)
        numStepsOpen = step - dist - 1
        subScore = getMaxP2 vMap pMap memHelper numStepsOpen v (Set.delete v remaining)
    scoresSelf = Set.map checkNeighbor remaining
    scoreElf = memHelper 26 start (Set.toAscList remaining)
    joinedScores = Set.insert scoreElf scoresSelf
    maxChild =
      if (not . null) remaining
        then Set.findMax joinedScores
        else 0

runGetMaxP2 :: ValveMap -> Int
runGetMaxP2 vMap = getMaxP2 filtedVMap pMap getMaxMem 26 start (Set.fromList (Map.keys filtedVMap))
  where
    pMap = calcPathLengths vMap
    filtedVMap = Map.filter (\v -> vRate v /= 0) vMap
    getMaxMem = getMaxScoreMem filtedVMap pMap

part1 :: IO ()
part1 = do
  print "part1"
  input <- toVMap <$> readInputLinesParser parseValve
  print $ runGetMax input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- toVMap <$> readInputLinesParser parseValve
  print $ runGetMaxP2 input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
