module Difference where
import qualified Data.Set as Set

difference :: (Ord a, Eq a) => [a] -> [a] -> [a]
difference a b =
    let bSet = Set.fromList b
    in  filter (\x -> not (x `Set.member` bSet)) a