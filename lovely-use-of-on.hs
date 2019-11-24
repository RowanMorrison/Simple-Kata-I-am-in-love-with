module Codewars.Kata.FixedXor where

import Data.Char
import Data.Bits
import Data.Function

fixedXor :: String -> String -> String
fixedXor = zipWith ((\x y -> intToDigit $ x `xor` y) `on` digitToInt)