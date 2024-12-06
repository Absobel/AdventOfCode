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

data Monotony = No | Incr | Decr deriving (Eq)

mononotony :: [Int] -> Monotony
mononotony l
  | sorted == l = Incr
  | reverse sorted == l = Decr
  | otherwise = No
  where
    sorted = sort l

hasRightInterval :: [Int] -> Bool
hasRightInterval [_] = True
hasRightInterval (h1 : h2 : t) =
  let s = abs (h1 - h2)
   in s > 0 && s < 4 && hasRightInterval (h2 : t)

isSafe :: [Int] -> Bool
isSafe l = (mononotony l /= No) && hasRightInterval l

allRemoveOne :: [Int] -> [[Int]]
allRemoveOne l = l : [take i l ++ drop (i + 1) l | i <- [0 .. length l - 1]]

main :: IO ()
main = print . length . filter (/= 0) . map (length . filter isSafe . allRemoveOne) . inputToList =<< parseFileLines "input/full.txt"
