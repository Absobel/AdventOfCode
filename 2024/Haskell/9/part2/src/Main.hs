module Main where

import Debug.Trace (trace, traceShow)
import Data.Char (digitToInt, isSpace, isDigit, intToDigit)
import Data.Maybe
import Data.List
import Parser

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

data Type = File Int | Free deriving (Show, Eq)

parseInput :: [Int] -> [(Type, Int)]
parseInput = zipWith (\i n -> (if even i then File (i `div` 2) else Free, n)) [0..]

gravity :: [(Type, Int)] -> [(Type, Int)]
gravity l = foldr (\idx cl -> putInFirstFree (fileIdx idx cl) cl) l [0..maxIdx l]

maxIdx :: [(Type, Int)] -> Int
maxIdx l = let (q, (t,_)) = fromJust $ unsnoc l in case t of Free -> maxIdx q ; File fi -> fi

fileIdx :: Int -> [(Type, Int)] -> (Type, Int)
fileIdx idx ((t, n):q) = case t of Free -> fileIdx idx q; File fi -> if fi == idx then (t, n) else fileIdx idx q

putInFirstFree :: (Type, Int) -> [(Type, Int)] -> [(Type, Int)] 
putInFirstFree e l | (File fi, n) <- e, ((tt, tn):q) <- l = case tt of File tfi -> if File fi == tt then if n == -1 then (Free, tn):q else l else (tt, tn):putInFirstFree e q ; Free -> if n == -1 || tn < n then (tt, tn):putInFirstFree e q else (File fi, n):(Free, tn-n):putInFirstFree (File fi, -1) q

gravitiedScore :: [(Type, Int)] -> Int
gravitiedScore l = snd $ foldr (\(t, n) (iacc, sacc) -> case t of Free -> (iacc+n, sacc) ; File fi -> (iacc+n, sacc + fi*(n*(2*iacc+n-1)) `div` 2)) (0, 0) (reverse l)

main :: IO ()
main = print . gravitiedScore . gravity . parseInput . map digitToInt . rstrip =<< readFile "input/full.txt"