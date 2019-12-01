
import Data.List (maximumBy)
import Data.Function (on)
import Data.List.Split (chop)
import Control.Arrow (first)

longest :: String -> String
longest = maximumBy (compare `on` length) . chop (splitBy (<=))

splitBy :: (a -> a -> Bool) -> [a] -> ([a],[a])
splitBy eq (x:y:zs) | eq x y = first (x :) $ splitBy eq (y:zs)
                    | otherwise = ([x],y:zs)
splitBy _ xs = (xs,[])