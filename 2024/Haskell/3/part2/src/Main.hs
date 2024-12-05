module Main where

import Debug.Trace (trace)
import Parser
import GHC.Base

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

type Mul = (Int, Int)
data D = Do | Dont deriving Show

parseMul :: Parser Mul
parseMul = (parseStr "mul(" *> parseInt <* parseStr ",") `pp` parseInt <* parseStr ")"

parseD :: Parser D
parseD = (Do <$ parseStr "do()") <|> (Dont <$ parseStr "don't()")

mul :: Mul -> Int
mul (a, b) = a*b

removeDont :: [Either Mul D] -> [Mul]
removeDont = removeDontAux True
removeDontAux _ [] = []
removeDontAux b (h:t) = case h of
    Left m -> if b then m:removeDontAux b t else removeDontAux b t
    Right d -> case d of
        Do -> removeDontAux True t
        Dont -> removeDontAux False t

main :: IO ()
-- main = print . consume (parseChar 'x' `pp` parseStr "mul(" `dp` parseInt `pd` parseStr ",") =<< readFile "input/test.txt"
main = print . (sum . map mul . removeDont <$>). maybeParse . fst . consume (many $ discardToParse $ eitherParse parseMul parseD) =<< readFile "input/full.txt"