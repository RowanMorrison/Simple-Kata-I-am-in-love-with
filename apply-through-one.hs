module Codewars.Kata.PlayPass where
import Data.Char
playPass :: String  -> Int -> String
playPass str n = reverse . zipWith ($) (cycle [toUpper, toLower]) . map f $ str
  where f x | isAlpha x = toEnum $ mod (ord x - 65 + n) 26 + 65
        f x | isDigit x = intToDigit . (9 -) . digitToInt $ x
        f x = x