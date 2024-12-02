module Main where

import Data.List
import Debug.Trace (trace)

parseFile :: FilePath -> IO [String]
parseFile path = do
  content <- readFile path
  return (lines content)

unzipList :: [[a]] -> [[a]]
unzipList [] = []
unzipList xs = [map (\(h : _) -> h) xs, map (\(_ : i : _) -> i) xs]

inputToList :: [String] -> [[Int]]
inputToList ls = unzipList (map (map read . words) ls)

count :: Eq a => [a] -> a -> Int
count l x = foldr (\h -> (+) (if h == x then 1 else 0)) 0 l

debugPrint :: Show a => a -> a
debugPrint x = trace ("Intermediate : " ++ show x) x

main :: IO ()
main = do
  l1 : l2 : _ <- map sort . inputToList <$> parseFile "input/full.txt"
  print $ sum $ map (\i -> i * count l2 i) l1