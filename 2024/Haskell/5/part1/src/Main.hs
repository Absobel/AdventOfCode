module Main where

import Debug.Trace (trace)
import Control.Applicative (Alternative(many))
import Parser
import qualified Data.Maybe as Maybe
import Data.Map (Map)
import qualified Data.Map as Map

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

---------------

parseRules :: Parser [(Int, Int)]
parseRules = many $ discardToParse ((parseInt <* parseChar '|') `pp` parseInt) 

parseUpdate :: Parser [[Int]]
parseUpdate = many $ discardToParse (concatParsers (many (parseInt <* parseChar ',')) (return <$> parseInt))

parseInput :: Parser ([(Int, Int)], [[Int]])
parseInput = parseRules `pp` parseUpdate

----------------

type MultiMap k v = Map k [v]

mLookup :: Ord k => k -> MultiMap k v -> [v]
mLookup k m = Maybe.fromMaybe [] (Map.lookup k m)

mFromList :: Ord k => [(k, v)] -> MultiMap k v
mFromList = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

----------------

mainUnCurry :: ([(Int, Int)], [[Int]]) -> Int
mainUnCurry (r, lu) = 
    let m = mFromList r in
    sum $ Maybe.mapMaybe (validUpdate m) lu

validUpdate :: MultiMap Int Int -> [Int] -> Maybe Int
validUpdate m u = if isUpdateValid m u then Just $ middle u else Nothing

middle :: [a] -> a
middle [x] = x
middle l = middle $ init $ tail l

isUpdateValid :: MultiMap Int Int -> [Int] -> Bool
isUpdateValid m [x] = True
isUpdateValid m (t1:t2:q) = t2 `elem` mLookup t1 m && isUpdateValid m (t2:q)

main :: IO ()
main = print . mainUnCurry . consumeRawUnsafe parseInput =<< readFile "input/full.txt"