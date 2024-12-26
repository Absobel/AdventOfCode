module Parser where

import Control.Applicative (Alternative)
import Data.Char (chr)
import GHC.Base

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

instance Alternative Parser where
  empty = P (Left "empty",)
  (<|>) (P fa) (P fb) = P $ \s ->
    case fa s of
      (Left err, res) -> fb s
      (Right a, res) -> (Right a, res)
  
-- Operations

pp :: Parser a -> Parser b -> Parser (a, b)
pp pa pb = (,) <$> pa <*> pb

concatParsers :: Parser [a] -> Parser [a] -> Parser [a]
concatParsers a1 a2 = (++) <$> a1 <*> a2

discardToParse :: Parser a -> Parser a
discardToParse (P fa) = P fla
  where
    fla s = case fa s of
      (Left err, res) ->
         let (_:ns) = s in
         if res == "" then (Left err, ns) else fla ns
      (Right a, res) -> (Right a, res)

-- Parsers

parseChar :: Char -> Parser Char
parseChar c = satisfy (== c)

parseStr :: String -> Parser String
parseStr "" = pure ""
parseStr (c : sc) = (:) <$> parseChar c <*> parseStr sc

parseInt :: Parser Int
parseInt = read <$> some (satisfy isDigit)

-- Other

satisfy :: (Char -> Bool) -> Parser Char
satisfy cb = P $ \case
  [] -> (Left "EOF", [])
  (sc : t)
    | cb sc -> (Right sc, t)
    | otherwise -> (Left "parser fails", t)

consume :: Parser a -> String -> (Either Error a, String)
consume (P fa) = fa

consumeMaybe :: Parser a -> String -> Maybe a
consumeMaybe (P fa) s = case fa s of
  (Left _, _) -> Nothing
  (Right x, _) -> Just x

consumeRawUnsafe :: Parser a -> String -> a
consumeRawUnsafe (P fa) s = case fa s of
  (Right x, _) -> x


eitherParse :: Parser a -> Parser b -> Parser (Either a b)
eitherParse (P fa) pb = P $ \s ->
  case fa s of
    (Left err, res) -> consume (Right <$> pb) s
    (Right a, res) -> (Right $ Left a, res)

-- Utils

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'