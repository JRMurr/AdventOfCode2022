module Utils.MapUtils where

import Data.Map
import qualified Data.Map as Map

-- | Update a value in the map allowing for a return value of the mapper to be returned to the caller
--
--
-- >>> mapEntrySideEffect 'a' (\x -> (x, x+1)) (Map.fromList [('a',0)])
-- (0,fromList [('a',1)])
mapEntrySideEffect :: Ord k => k -> (v -> (r, v)) -> Map k v -> (r, Map k v)
mapEntrySideEffect key f m = (res, Map.insert key mappedVal m)
  where
    value = m ! key
    (res, mappedVal) = f value

-- | Update a value in the map given a key
--
-- >>> mapEntry 'a' (+1) (Map.fromList [('a',0)])
-- fromList [('a',1)]
mapEntry :: Ord k => k -> (v -> v) -> Map k v -> Map k v
mapEntry key f m = let (_, res) = mapEntrySideEffect key (\x -> (Nothing, f x)) m in res
