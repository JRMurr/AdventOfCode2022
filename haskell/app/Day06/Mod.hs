module Day06.Mod where

import qualified Data.Set as Set
-- import qualified Debug.Trace as Debug
import Utils.Mod

--  the start of a packet is indicated by a sequence of four characters that are all different.

-- Whoops... thought it was first idx where the 4th new char is seen.....
-- at least now i "get" the state mondad

-- findStart :: String -> State (Set Char, Int) Int
-- findStart [] = error "empty"
-- findStart (x : xs) = do
--   (seen, idx) <- get
--   if Debug.trace (show seen) $ Set.size seen == 4
--     then return idx
--     else
--       ( do
--           _ <- put (Set.insert x seen, idx + 1)
--           findStart xs
--       )

-- getStartIdx :: String -> Int
-- getStartIdx s = evalState (findStart s) (Set.empty, 0)

findFirstXChunk :: Int -> String -> Int
findFirstXChunk n s = head [idx + n | (idx, group) <- enumerate groups, Set.size (Set.fromList group) == n]
  where
    groups = chunkOverlap n s

part1 :: IO ()
part1 = do
  print "part1"
  input <- map (findFirstXChunk 4) <$> readInputLinesMapper id
  print input
  return ()

part2 :: IO ()
part2 = do
  print "part2"
  input <- map (findFirstXChunk 14) <$> readInputLinesMapper id
  print input
  return ()

dispatch :: [(Int, IO ())]
dispatch = [(1, part1), (2, part2)]
