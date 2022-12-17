module Day16.Mod where

import Data.List (permutations)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Debug.Trace as Debug
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod

parseInt :: Parser Int
parseInt = label "integer" $ read <$> (some numberChar <|> ((:) <$> char '-' <*> some numberChar))

parseVName :: Parser VName
parseVName = label "vName" $ Vname <$> many letterChar

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

newtype VName = Vname String deriving (Show, Ord, Eq)

data Valve = V {vName :: VName, vRate :: Int, vTunnels :: [VName]} deriving (Show)

type ValveMap = Map VName Valve

type Path = (VName, VName) -- (start,end)

type PathLengths = Map Path Int

toVMap :: [Valve] -> ValveMap
toVMap valves = Map.fromList [(vName v, v) | v <- valves]

-- could just be a bool but ehh
-- data CurrentValve = Opening Valve | StandingAt Valve deriving (Show)

-- data Node = N {openValves :: [Valve], currentValve :: CurrentValve, pressureReleased :: Int}

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

type Node = ([VName], VName, Int)

-- TODO: gotta have an off by one on step math or something
getMaxScore :: ValveMap -> PathLengths -> Int -> Node -> Int
getMaxScore vMap pMap step (visited, curr, currFlowRate)
  -- these cases might be weird
  | step > 30 = 0
  | step == 30 = currFlowRate
  | otherwise = if null scores then (30 - step) * currFlowRate else maximum scores
  where
    remaining = filter (`notElem` visited) $ Map.keys vMap
    checkNeighbor v = flowAddTill + getMaxScore vMap pMap stepWithOpen (v : visited, v, currFlowRate + neighborRate)
      where
        neighborRate = vRate $ vMap ! v
        numMovesTo = pMap ! (curr, v)
        flowAddTill = (1 + numMovesTo) * currFlowRate -- might not need the 1+
        stepWithOpen = 1 + step + numMovesTo -- TODO: what if this goes over 30?
    scores = map checkNeighbor remaining

runGetMax :: ValveMap -> Int
runGetMax vMap = getMaxScore filtedVMap pMap 0 ([start], start, 0)
  where
    pMap = calcPathLengths vMap
    filtedVMap = Map.filter (\v -> vRate v /= 0) vMap
    start = Vname "AA"

part1 :: IO ()
part1 = do
  print "part1"
  input <- toVMap <$> readInputLinesParser parseValve
  print $ runGetMax input
  -- print $ Map.elems input
  -- printNewLine $ Map.elems input
  -- print $ length $ getAllOrderings input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- toVMap <$> readInputLinesParser parseValve
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
