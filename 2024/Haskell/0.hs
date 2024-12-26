module Main where

import Debug.Trace (trace)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

main :: IO ()
main = print . lines =<< readFile "input/test.txt"