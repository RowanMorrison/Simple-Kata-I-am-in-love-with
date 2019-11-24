{-# LANGUAGE StandaloneDeriving, RankNTypes #-}

module Hanabi where
import Preloaded (Suit(..))
import Data.List

deriving instance Ord Suit

ways :: [(Int, Suit)] -> Int
ways vs = let combine :: forall a. (Eq a, Ord a) => [a] -> Int
              combine = length . map head . group . sort
          in combine (map fst vs) + combine (map snd vs)