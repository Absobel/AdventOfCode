module Main where

import Control.Applicative (Alternative ((<|>)), many)
import qualified Data.Bifunctor
import Data.Maybe (isJust, fromJust)
import Debug.Trace (trace, traceShow)
import Parser
import Data.List
import Data.List.Extra

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

type Line = (Int, [Int])

data Op = Add | Mul | Cat deriving (Show, Eq)

parserLines :: Parser [Line]
parserLines = many ((parseInt <* parseStr ": ") `pp` concatParsers (many (parseInt <* parseChar ' ')) (return <$> (parseInt <* parseChar '\n')))

-- operation -> (predicat, inverse)
inverseOp :: (Show a, Read a, Num a, Ord a, Integral a) => Op -> (a -> a -> Bool, a -> a -> a)
inverseOp Add = ((>=), (-))
inverseOp Mul = (\a b -> mod a b == 0, div)
-- inverse de reverse cat et pas juste reverse de cat parce que lazy et je prneds l'input à l'envers
inverseOp Cat = (\a b -> show b `isSuffixOf` show a, \a b -> read $ fromJust $ stripSuffix (show b) (show a))

compteEstBonOneOp :: Line -> Op -> Maybe [Op]
compteEstBonOneOp (r, t1 : q) op = let (p, iop) = inverseOp op in if p r t1 then (:) op <$> compteEstBon (iop r t1, q) else Nothing

compteEstBon :: Line -> Maybe [Op]
compteEstBon (r, [x]) = if x == r then Just [] else Nothing
compteEstBon l = compteEstBonOneOp l Add <|> compteEstBonOneOp l Mul <|> compteEstBonOneOp l Cat

main :: IO ()
main = print . sum . map fst . filter (\(_, l) -> isJust l) . map (\(r, l) -> (r, compteEstBon (r, reverse l))) . consumeRawUnsafe parserLines =<< readFile "input/full.txt"