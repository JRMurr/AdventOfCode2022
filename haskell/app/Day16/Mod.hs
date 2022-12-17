module Day16.Mod where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Debug.Trace as Debug
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Mod
import Data.List (permutations)
import Data.Maybe (mapMaybe)

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

getPathLength :: PathLengths-> Path -> Int
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

-- just get all possible paths from AA, ignore steps for now
-- TODO: this is slow
getAllOrderings :: ValveMap -> [[VName]]
getAllOrderings vMap = map (Vname "AA": ) (permutations validValves)
  where
    allValves = Map.elems vMap
    validValves = mapMaybe (\v -> if vRate v /= 0 then Just (vName v) else Nothing) allValves


part1 :: IO ()
part1 = do
  print "part1"
  input <- toVMap <$> readInputLinesParser parseValve
  -- print $ Map.elems input
  -- printNewLine $ Map.elems input
  print $ length $ getAllOrderings input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- toVMap <$> readInputLinesParser parseValve
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
