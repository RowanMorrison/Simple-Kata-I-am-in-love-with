solve _ [x] = x
solve k ls = solve k $ take (length ls - 1) $ drop k $ cycle ls

josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = solve k $ [1..n]