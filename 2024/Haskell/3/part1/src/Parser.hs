module Parser where

import Data.Char (chr)

type Error = String

newtype Parser a = P (String -> (Either Error a, String))

instance Functor Parser where
  fmap ab (P fa) = P $ \s ->
    case fa s of
      (Left err, res) -> (Left err, res)
      (Right a, res) -> (Right $ ab a, res)

instance Applicative Parser where
  pure a = P (Right a,)
  (<*>) (P fab) (P fa) = P $ \s ->
    case fab s of
      (Left err, res) -> (Left err, res)
      (Right ab, res) -> case fa res of
        (Left err, res) -> (Left err, res)
        (Right a, res) -> (Right $ ab a, res)

instance Monad Parser where
  (>>=) (P fa) famb = P $ \s ->
    case fa s of
      (Left err, res) -> (Left err, res)
      (Right a, res) -> let P f = famb a in f res

-- Operations

pp :: Parser a -> Parser b -> Parser (a, b)
pp pa pb = (,) <$> pa <*> pb

concatParsers :: Parser [a] -> Parser [a] -> Parser [a]
concatParsers a1 a2 = (++) <$> a1 <*> a2

-- 0 or more
manyParse :: Parser a -> Parser [a]
manyParse (P fa) = P fla
  where
    fla s = case fa s of
      (Left err, _) -> (Right [], s)
      (Right a, res) -> case fla res of
        (Left err, res2) -> (Left err, res2)
        (Right la, res2) -> (Right (a : la), res2)

someParse :: Parser a -> Parser [a]
someParse (P fa) = P $ \s ->
  case fa s of
    (Left err, res) -> (Left err, res)
    (Right a, res) -> consume ((a :) <$> manyParse (P fa)) res

discardToParse :: Parser a -> Parser a
discardToParse (P fa) = P fla
  where
    fla s = case fa s of
      (Left err, res) -> if res == "" then (Left err, res) else fla res
      (Right a, res) -> (Right a, res)

-- Parsers

parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

parseStr :: String -> Parser String
parseStr "" = pure ""
parseStr (c : sc) = (:) <$> parseChar c <*> parseStr sc

parseInt :: Parser Int
parseInt = read <$> someParse (satisfy isDigit)

-- Other

satisfy :: (Char -> Bool) -> Parser Char
satisfy cb = P $ \case
  [] -> (Left "EOF", [])
  (sc : t)
    | cb sc -> (Right sc, t)
    | otherwise -> (Left "parser fails", t)

consume :: Parser a -> String -> (Either Error a, String)
consume (P fa) = fa

-- Utils

concatInt :: [Int] -> Int
concatInt [a] = a
concatInt (i : q) = read (show i ++ show (concatInt q))

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

result :: Either Error a -> Maybe a
result = either (const Nothing) Just