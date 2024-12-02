module Main where

import Data.List
import Debug.Trace (trace)

parseFileLines :: FilePath -> IO [String]
parseFileLines path = do
  content <- readFile path
  return (lines content)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

inputToList :: [String] -> [[Int]]
inputToList = map (map read . words)

data Monotony = No | Incr | Decr
mononotony :: [Int] -> Monotony
mononotony l
  | sort l == l = Incr
  | sortOn (\x -> -x) l == l = Decr
  | otherwise = No

hasRightInterval :: [Int] -> Bool
hasRightInterval [_] = True
hasRightInterval (h1 : h2 : t) = let s = abs (h1 - h2) in 
    s > 0 && s < 4 && hasRightInterval (h2:t)

isSafe :: [Int] -> Bool
isSafe l = (case mononotony l of No -> False; _ -> True) && hasRightInterval l

main :: IO ()
main = print . length . filter isSafe . inputToList =<< parseFileLines "input/full.txt"