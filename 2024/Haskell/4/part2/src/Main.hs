module Main where

import Debug.Trace (trace)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

arrLetter :: [[a]] -> Int -> Int -> a
arrLetter l x y = (l !! x) !! y

isCoordValid :: [String] -> Int -> Int -> Bool
isCoordValid l x y = let a = arrLetter l x y in (a /= '.') && (a == 'A') && (let m = concatMap (\i -> map (\j -> arrLetter l (x + i) (y + j)) [-1, 1]) [-1, 1] in m `elem` [['S', 'S', 'M', 'M'], ['M', 'S', 'M', 'S'], ['M', 'M', 'S', 'S'], ['S', 'M', 'S', 'M']])

nbValid :: [String] -> Int
nbValid l = length (filter id (concat $ zipWith (\i s -> zipWith (\j _ -> isCoordValid l i j) [0 ..] s) [0 ..] l))

pad :: [String] -> [String]
pad l = let len = length $ head l in let p = (concat $ replicate (len + 2) ".") in p : map (\s -> "." ++ s ++ ".") l ++ [p]

main :: IO () -- todo : mettre coord a chaque A, compter les duplicates
main = print . nbValid . pad . lines =<< readFile "input/full.txt"