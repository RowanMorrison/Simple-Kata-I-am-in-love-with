import Data.List (transpose, unfoldr)

data Tree a = Nil | Node {left :: Tree a, value :: a, right :: Tree a} deriving (Show)

treeTop :: Tree Int -> [Int]
treeTop Nil = []
treeTop root = leftRun ++ rightRun where
  run _ Nil = Nothing
  run f node = Just (value node, f node)
  leftRun = reverse $ tail $ unfoldr (run left) root
  rightRun = unfoldr (run right) root
