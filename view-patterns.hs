{-# Language ViewPatterns #-}

module AreaOfPolygonInsideCircle (areaOfPolygonInsideCircle) where

precision :: (Integral a, RealFrac b) => a -> b -> b
precision p n = fromIntegral (round (n * 10^p)) / 10^p

areaOfPolygonInsideCircle :: Double -> Int -> Double
areaOfPolygonInsideCircle r (fromIntegral -> n) = precision 3 $ n * r * r * sin (pi/n) * cos (pi/n)