module Main where

-- need to update other-modules to add sub modules

import qualified Day00.Mod as Day00 (dispatch)
import qualified Day01.Mod as Day01 (dispatch)
import qualified Day02.Mod as Day02 (dispatch)
import qualified Day03.Mod as Day03 (dispatch)
-- Add day import

import System.Environment (getArgs)

type DayDispatcher = [(Int, IO ())]

dayMap :: [(Int, DayDispatcher)]
dayMap =
  [ -- map of day num to thats days parts
    (0, Day00.dispatch),
    (1, Day01.dispatch),
    (2, Day02.dispatch),
    (3, Day03.dispatch)
    -- Add day dispatch
  ]

main :: IO ()
main = do
  print "here"
  day : part : _ <- getArgs
  let (Just dayDispatcher) = lookup (read day) dayMap
  let (Just func) = lookup (read part) dayDispatcher
  func
