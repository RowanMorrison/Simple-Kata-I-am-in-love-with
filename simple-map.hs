import qualified Data.Map as M

frequency :: Ord a => [a] -> [(a, Int)]
frequency = M.toAscList . foldr (\x -> M.insertWith (+) x 1) M.empty
