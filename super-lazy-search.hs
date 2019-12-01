module ShortestList where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)

shortestList :: [[a]] -> [a]
shortestList [] = []
shortestList xs = xs !! (indexShortest xs)
  where indexShortest xs = fromMaybe (indexShortest (map tail xs)) (findIndex null xs)