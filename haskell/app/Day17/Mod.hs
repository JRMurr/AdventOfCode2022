module Day17.Mod where

import Data.List (maximumBy, minimumBy, uncons)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Debug.Trace as Debug
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils.Coords
import Utils.Mod
import Prelude hiding (Left, Right)

-- list of coords relative to the top left position of the shape
newtype FallingRock = R (Set Coord) deriving (Show)

-- negate all y cords so the top left is 0 for falling
makeRock :: [Coord] -> FallingRock
makeRock c = R $ Set.fromList (map (\(C y x) -> C (y * (-1)) x) c)

hLine, plus, lRock, vLine, square :: FallingRock
hLine = makeRock [C 0 x | x <- [0 .. 3]]
plus = makeRock (C 1 0 : C 1 2 : [C y 1 | y <- [0 .. 2]])
lRock = makeRock (C 2 0 : C 2 1 : [C y 2 | y <- [0 .. 2]])
vLine = makeRock [C y 0 | y <- [0 .. 3]]
square = makeRock [C y x | (x, y) <- cartProd [0, 1] [0, 1]]

addRockCord :: FallingRock -> Coord -> FallingRock
addRockCord (R points) offset = R $ Set.map (addCoord offset) points

spawnRock :: FallingRock -> Int -> FallingRock
spawnRock r@(R points) highestGridPoint =
  -- Debug.trace ("(g, y, off)" ++ show (highestGridPoint, lowestYInRock, offset)) $
  addRockCord r offset
  where
    lowestYInRock = coordRow $ Set.findMin points
    offset = C (3 + highestGridPoint + ((-1) * lowestYInRock)) 2

leftEdge, rightEdge :: FallingRock -> Int
leftEdge (R points) = let (C _ x) = minimumBy compareCols points in x
rightEdge (R points) = let (C _ x) = maximumBy compareCols points in x

rockOrder :: [FallingRock]
rockOrder = cycle [hLine, plus, lRock, vLine, square]

-- getRockForStep :: Int -> FallingRock
-- getRockForStep n = rockOrder !! (n `mod` length rockOrder)

data Move = Left | Right deriving (Show, Eq)

parseMove :: Parser Move
parseMove = Left <$ char '<' <|> Right <$ char '>'

parseLine :: Parser [Move]
parseLine = cycle <$> many parseMove

type Grid = Set Coord

highestRow :: Grid -> Int
highestRow g
  | null g = 1
  | otherwise = 1 + coordRow (Set.findMax g) -- coord sorts by row

moveRock :: Move -> FallingRock -> FallingRock
moveRock m r@(R points)
  | m == Left && leftEdge r == 0 = r
  | m == Right && rightEdge r == 6 = r
  | otherwise = R (Set.map moveF points)
  where
    moveF = if m == Left then left else right

rockIntersects :: Grid -> FallingRock -> Bool
rockIntersects g (R points) = (not . null) $ Set.intersection points g

-- apply the move to the rock then drop it
-- if it would rest return true for the 2nd arg, otherwise false
stepRock :: Grid -> Move -> FallingRock -> (FallingRock, Bool)
stepRock g m rock
  | intersects droppedRock = (rockToDrop, True)
  | otherwise = (droppedRock, False)
  where
    intersects = rockIntersects g
    moved = moveRock m rock
    rockToDrop = if intersects moved then rock else moved
    droppedRock = addRockCord rockToDrop (C (-1) 0)

-- sim the rock till it stops falling
simRock :: Grid -> [Move] -> FallingRock -> (Grid, [Move])
simRock g moves rock = (Set.union rockCoords g, remaningMoves)
  where
    loop _ [] = error "invalid moves"
    loop r (m : ms) =
      let (dropped, done) = stepRock g m r
       in if done then (dropped, ms) else loop dropped ms
    (R rockCoords, remaningMoves) = loop rock moves

runStep :: Grid -> [Move] -> FallingRock -> (Grid, [Move])
runStep g m initRock = simRock g m rockCoords
  where
    -- initRock = getRockForStep step
    rockCoords = spawnRock initRock (highestRow g)

runXSteps :: Int -> Grid -> [Move] -> [FallingRock] -> Grid
runXSteps 0 g _ _ = g
runXSteps n g m rocks = runXSteps (n - 1) newGrid newMoves rs
  where
    Just (r, rs) = uncons rocks
    (newGrid, newMoves) = runStep g m r

runHelper :: (Grid, [Move], [FallingRock]) -> (Grid, [Move], [FallingRock])
runHelper (g, m, rocks) = (newGrid, newMoves, rs)
  where
    Just (r, rs) = uncons rocks
    (newGrid, newMoves) = runStep g m r

initGrid :: Set Coord
initGrid = Set.fromList [C (-1) x | x <- [0 .. 6]]

run :: [Move] -> Int -> Grid
run m n = g
  where
    (g, _, _) = iterate runHelper (initGrid, m, rockOrder) !! n

drawGrid :: Grid -> String
drawGrid g = drawCoordSet $ Set.map (\(C y x) -> C (y * (-1)) x) g

part1 :: IO ()
part1 = do
  print "part1"
  input <- head <$> readInputLinesParser parseLine
  -- print $ take
  let res = run input 2022
  print $ highestRow res
  -- let res = run input 9
  -- putStrLn $ drawGrid res
  -- putStrLn $ drawGrid (let (R p) = lRock in p)
  -- print $ take 30 input
  return ()

-- TODO: look for cycles and cache them
part2 :: IO ()
part2 = do
  print "part2"
  input <- head <$> readInputLinesParser parseLine
  let res = run input 1000000000000
  print $ highestRow res
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
