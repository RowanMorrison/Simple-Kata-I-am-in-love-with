tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci sig n = take n $ unfoldr (\(a,b,c) -> Just (a,(b,c,a+b+c))) sig