possibilities :: String -> [String]
possibilities = traverse (\case '?' -> "01"; x -> [x])
