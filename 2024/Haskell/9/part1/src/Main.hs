module Main where

import Debug.Trace (trace, traceShow)
import Data.Char (digitToInt, isSpace)
import Data.Maybe
import Data.List

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

dPrintF f x = trace (show $ f x) x

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

parseInput :: [Int] -> [Int]
parseInput l = concat $ zipWith (\i n -> replicate n (if even i then i `div` 2 else -1)) [0..] l

-- replace anything that follows the predicate in the first list by the second list the best it can starting by the beginning
replace :: (a -> Bool) -> [a] -> [a] -> [a] 
replace _ [] _ = []
replace p (t1:q1) (t2:q2) = if p t1 then if p t2 then replace p (t1:q1) q2 else t2:replace p q1 q2 else t1:replace p q1 (t2:q2)

gravitiedScore :: [Int] -> Int
gravitiedScore l = foldr (\(i, e) acc -> acc + i*e) 0 (zip [0..] l)

main :: IO ()
main = print . gravitiedScore . (\l -> take (length $ filter (-1 /=) l) $ replace (-1 ==) l (reverse l)) . parseInput . map digitToInt . rstrip =<< readFile "input/full.txt"