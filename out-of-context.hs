cycleS :: [a] -> Stream a
cycleS xs =  (foldr (:>) (cycleS xs)  xs)

(<*>) = zipWithS ($)