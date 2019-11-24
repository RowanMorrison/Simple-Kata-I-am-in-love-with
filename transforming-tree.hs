{-# LANGUAGE DeriveFoldable, LambdaCase #-}

module TreeTop where

    import Data.Foldable (toList)
    
    data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Foldable, Show)
    
    treeTop :: Tree Int -> [Int]
    treeTop = toList . transform left right
      where
        transform f g = \case
          Nil        -> Nil
          Node l x r -> Node (f l) x (g r)
        
        nil   = const Nil
        left  = transform left nil
        right = transform nil  right
