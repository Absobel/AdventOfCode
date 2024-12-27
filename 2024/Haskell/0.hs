module Main where

import Debug.Trace (trace, traceShow)

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

main :: IO ()
main = print . lines =<< readFile "input/test.txt"