module Day16.Mod where

import Data.List (maximumBy)
import Data.Map (Map, (!))
import qualified Data.Map as Map
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

getMaxScore :: ValveMap -> PathLengths -> Int -> Visited -> (Int, [VName])
getMaxScore vMap pMap step visited
  | step < 0 = error $ "too many steps: " ++ show step
  | step == 0 = (0, visited)
  | otherwise = maxChild
  where
    curr = head visited
    remaining = filter (`notElem` visited) $ Map.keys vMap
    checkNeighbor v =
      if dist >= step
        then (0, visited)
        else ((numStepsOpen * neighborRate) + subScore, subVis)
      where
        neighborRate = vRate $ vMap ! v
        dist = pMap ! (curr, v)
        numStepsOpen = step - dist - 1
        (subScore, subVis) = getMaxScore vMap pMap numStepsOpen (v : visited)
    scores = map checkNeighbor remaining
    maxChild =
      if (not . null) remaining
        then maximumBy (\(s1, _) (s2, _) -> compare s1 s2) scores
        else (0, visited)

runGetMax :: ValveMap -> (Int, [VName])
runGetMax vMap = (score, reverse vis)
  where
    pMap = calcPathLengths vMap
    filtedVMap = Map.filter (\v -> vRate v /= 0) vMap
    start = Vname "AA"
    (score, vis) = getMaxScore filtedVMap pMap 30 [start]

part1 :: IO ()
part1 = do
  print "part1"
  input <- toVMap <$> readInputLinesParser parseValve
  -- let pMap = calcPathLengths input
  -- printNewLine $ Map.toList input

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
