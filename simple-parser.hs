module Smile where

newtype Parser a = Parser { parse :: String -> [(a,String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return a = Parser (\s -> [(a,s)])
  (>>=) p f  = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

runParser :: Parser a -> String -> Bool
runParser m s =
  case parse m s of
    [(res, [])] -> True
    [(_, rs)]   -> False
    _           -> False

parseChar :: [Char] -> Parser Char
parseChar cs = Parser $ \s -> 
  case s of
    (c:s') | c `elem` cs -> [(c, s')]
    _                    -> []

parseCharOpt :: [Char] -> Parser (Maybe Char)
parseCharOpt cs = Parser $ \s -> 
  case s of
    (c:s') | c `elem` cs -> [(Just c, s')]
    _                    -> [(Nothing, s)]

parseEye  = parseChar [':', ';']
parseNose = parseCharOpt ['-', '~']
parseSmiley = parseChar [')', 'D']

isSmiley s = 
  runParser (parseEye >> parseNose >> parseSmiley) s

countSmileys :: [String] -> Int
countSmileys = length . filter isSmiley