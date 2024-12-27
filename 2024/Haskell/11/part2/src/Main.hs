module Main where

import Debug.Trace (trace, traceShow)
import Parser (consumeRaw, parseInt, discardToParse)
import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

mapair :: (a->b) -> (a,a) -> (b,b)
mapair f (a, b) = (f a, f b)

addStone :: (Int, Int) -> Map Int Int -> Map Int Int
addStone (n,o) = Map.insertWith (+) n o

blinkStone :: (Int, Int) -> Map Int Int
blinkStone (n, o)
    | n == 0 = Map.fromList [(1,o)]
    | even lsn = let (n1, n2) = mapair read $ splitAt (lsn `div` 2) sn in Map.fromList (if n1 == n2 then [(n1, 2*o)] else [(n1, o), (n2, o)])
    | otherwise = Map.fromList [(2024*n, o)]
    where 
        sn = show n
        lsn = length sn

blink :: Int -> Map Int Int -> Map Int Int
blink 0 l = l
blink n l = blink (n-1) (foldr (\l acc -> Map.unionWith (+) acc (blinkStone l)) Map.empty (Map.toList l))

main :: IO ()
main = print . sum . blink 75 . Map.fromList . map (,1) . consumeRaw (many $ discardToParse parseInt) =<< readFile "input/full.txt"