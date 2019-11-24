dropFromEnd :: Int -> [a] -> [a]
dropFromEnd = (zipWith const <*>) . drop
