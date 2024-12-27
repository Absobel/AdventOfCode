module Main where

import Debug.Trace (trace, traceShow)
import Parser (consumeRaw, parseInt, discardToParse)
import Control.Applicative

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

toList :: (a,a) -> [a]
toList (a,b) = [a, b]

blinkStone :: Int -> [Int]
blinkStone n
    | n == 0 = [1]
    | even lsn = map read $ toList $ splitAt (lsn `div` 2) sn
    | otherwise = [2024*n]
    where 
        sn = show n
        lsn = length sn

blink :: Int -> [Int] -> [Int]
blink 0 l = l
blink n l = blink (n-1) (concatMap blinkStone l)

main :: IO ()
main = print . length . blink 25 . consumeRaw (many $ discardToParse parseInt) =<< readFile "input/full.txt"