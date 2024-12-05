module Main where

import Data.List
import Debug.Trace (trace)

parseFile :: FilePath -> IO [String]
parseFile path = do
  content <- readFile path
  return (lines content)

unzipList :: [[a]] -> [[a]]
unzipList [] = []
unzipList xs = [map head xs, map (\(_ : i : _) -> i) xs]

zipList :: [[a]] -> [[a]]
zipList ([] : [] : _) = []
zipList ((h1 : t1) : (h2 : t2) : _) = [h1, h2] : zipList [t1, t2]
zipList _ = error "not possible"

inputToList :: [String] -> [[Int]]
inputToList ls = unzipList (map (map read . words) ls)

distance :: [Int] -> Int
distance (h : i : _) = abs (h - i)

debugPrint :: Show a => a -> a
debugPrint x = trace ("Intermediate : " ++ show x) x

main :: IO ()
main = print . sum . map distance . zipList . map sort . inputToList =<< parseFile "input/full.txt"