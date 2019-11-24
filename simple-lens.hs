import Control.Lens.Wrapped (ala)
import Data.Semigroup (Endo(..), stimes)

iterateN :: Int -> (a -> a) -> (a -> a)
iterateN = ala Endo . stimes