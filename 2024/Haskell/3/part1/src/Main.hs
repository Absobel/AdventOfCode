module Main where

import Debug.Trace (trace)
import Parser

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

type Mul = (Int, Int)

parseMul :: Parser Mul
parseMul = (parseStr "mul(" *> parseInt <* parseStr ",") `pp` parseInt <* parseStr ")"

mul :: Mul -> Int
mul (a, b) = a*b

consumeMuls :: String -> Either Error Int
consumeMuls s = case consume (manyParse parseMul) s of
        (Right lm, res) -> Right $ sum $ map mul lm
        (Left err, res2) -> Left err

main :: IO ()
-- main = print . consume (parseChar 'x' `pp` parseStr "mul(" `dp` parseInt `pd` parseStr ",") =<< readFile "input/test.txt"
main = print . (sum . map mul <$>) . result . fst . consume (manyParse $ discardToParse parseMul) =<< readFile "input/full.txt"