cycle                   :: [a] -> [a]
cycle []                = errorEmptyList "cycle"
cycle xs                = xs' where xs' = xs ++ xs'