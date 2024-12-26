module Main where

import Control.Applicative (Alternative ((<|>)), many)
import qualified Data.Bifunctor
import Data.Maybe (isJust)
import Debug.Trace (trace)
import Parser

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

type Line = (Int, [Int])

data Op = Add | Mul deriving (Show, Eq)

allOps = [Add, Mul]

parserLines :: Parser [Line]
parserLines = many ((parseInt <* parseStr ": ") `pp` concatParsers (many (parseInt <* parseChar ' ')) (return <$> (parseInt <* parseChar '\n')))

operation :: Num a => Op -> (a -> a -> a)
operation Add = (+)
operation Mul = (*)

-- operation -> (predicat, inverse)
inverseOp :: (Num a, Ord a, Integral a) => Op -> (a -> a -> Bool, a -> a -> a)
inverseOp Add = ((>=), (-))
inverseOp Mul = (\a b -> mod a b == 0, div)

compteEstBonOneOp :: Line -> Op -> Maybe [Op]
compteEstBonOneOp (r, t1 : q) op = let (p, iop) = inverseOp op in if p r t1 then (:) op <$> compteEstBon (iop r t1, q) else Nothing

compteEstBon :: Line -> Maybe [Op]
compteEstBon (r, [x]) = if x == r then Just [] else Nothing
compteEstBon l = compteEstBonOneOp l Add <|> compteEstBonOneOp l Mul

-- useless
compute :: [Int] -> [Op] -> Int
compute [x] [] = x
compute (t1 : q) (op : qop)
  | length (t1 : q) == length (op : qop) + 1 = operation op t1 (compute q qop)

main :: IO ()
main = print . sum . map fst . filter (\(_, l) -> isJust l) . map (\(r, l) -> (r, compteEstBon (r, reverse l))) . consumeRawUnsafe parserLines =<< readFile "input/full.txt"