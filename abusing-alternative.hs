module Apple (apple) where

import Apple.Preloaded (Tree(..)) -- data Tree = Null | Node { id :: Int, poisoned :: Bool, left :: Tree, right :: Tree }

import Control.Applicative ((<|>))

apple :: Tree -> Maybe Int
apple Null = Nothing
apple (Node n p l r) = apple l <|> apple r <|>
  case p of
    True  -> Just n
    False -> Nothing 