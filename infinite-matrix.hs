module Laziness where

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue m = head [(x, y) | (x, y) <- indexes, m !! x !! y]
  where
    indexes = [(x, y) | i <- [0..], x <- [0..i], y <- [0..i]]-- and mine!module Laziness where

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue = snd . head . filter fst . compress . label 0
  where    -- first we label each element in matrix with its coordinates
    label :: Int -> Matrix -> [[(Bool, (Int, Int))]]
    label n (xn:xys) = zip xn (zip (repeat n) [0..]) : label (succ n) xys 
          -- and then we diagonally compress it     --    -- 0 0 1 0    -- 0 0 1 0    -- 1 1 1 0    -- 0 0 0 0     -- etc.    --
    compress :: [[(Bool, (Int, Int))]] -> [(Bool, (Int, Int))]
    compress = go 1
      where
        go :: Int -> [[(Bool, (Int, Int))]] -> [(Bool, (Int, Int))]
        go n xys = 
          let (xs, xn:xss) = splitAt n xys
              free = fmap head xs ++ take n xn
          in  free ++ go (succ n) ((fmap tail xs) ++ [drop n xn] ++ xss)
    