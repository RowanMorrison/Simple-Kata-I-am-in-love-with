module Codewars.Kata.RemovNB where

import Data.Maybe (mapMaybe)

removNb :: Integer-> [(Integer, Integer)]
removNb n = mapMaybe f [1..n]
    where f x = (\(d, m) -> if m == 0 && d <= n then Just (x, d) else Nothing) ((n * (n+1) `div` 2 - x) `divMod` (x+1))


removNb :: Integer-> [(Integer, Integer)]
removNb n = reverse $ go [] 1
  where
    sum = n * (n + 1) `div` 2
  
    go xs i =
      if i < n 
        then 
           let (j, m) = (sum - i) `divMod` (1 + i)
           in
             if m == 0 && j < n
               then go ((i, j):xs) (succ i) 
               else go xs          (succ i)
        else 
          xs