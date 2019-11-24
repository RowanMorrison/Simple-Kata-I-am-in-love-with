module Codwars.Kata.Duplicates where

import qualified Data.Map.Strict as M
import Data.Char (toLower)

duplicateCount :: String -> Int
duplicateCount = 
    M.foldr (+) 0 . M.fromListWith (const . const 1) 
  . flip zip (repeat 0) . fmap toLower