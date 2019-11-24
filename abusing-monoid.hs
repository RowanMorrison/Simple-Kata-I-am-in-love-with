module High.JorgeVS.Kata where

  import Data.Monoid 
  import Data.Char (ord)
  import Data.Coerce
  
  newtype Score = Score { unScore :: String }
  
  instance Monoid Score where
    mempty = Score ""
    (Score s1) `mappend` (Score s2)
      | count s1 > count s2 = Score s1
      | otherwise           = Score s2
      where 
        count = foldr ((+) . num) 0
        num x = ord x - ord 'a' + 1
  
  high :: String -> String
  high = unScore . mconcat . coerce . words