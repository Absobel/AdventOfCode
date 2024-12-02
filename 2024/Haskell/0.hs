module Main where

import Debug.Trace (trace)

parseFileLines :: FilePath -> IO [String]
parseFileLines path = do
    content <- readFile path
    return (lines content)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

main :: IO ()
main = print =<< parseFileLines "input/full.txt"