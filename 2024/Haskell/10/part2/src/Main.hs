{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module Main where

import Debug.Trace (trace, traceShow)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (digitToInt)
import qualified Data.Maybe as Maybe
import Data.List

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

----------

type MultiMap k v = Map k [v]

mLookup :: Ord k => k -> MultiMap k v -> [v]
mLookup k m = Maybe.fromMaybe [] (Map.lookup k m)

mFromList :: Ord k => [(k, v)] -> MultiMap k v
mFromList = foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty

----------

type Coord = (Int, Int)

parseInput :: [[Char]] -> MultiMap Int Coord
parseInput l = mFromList $ concat (zipWith (\i s -> zipWith (\j d -> (digitToInt d, (i,j))) [0..] s) [0..] l)

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

nextPaths :: Int -> [[Coord]] -> MultiMap Int Coord -> [[Coord]]
nextPaths n cl m = let tails = map last cl in let nbo = map neighbors tails in let suiv = mLookup (n + 1) m in concat $ zipWith (\c nb -> map (\a -> c ++ [a]) $ filter (`elem` suiv) nb) cl nbo

nines :: MultiMap Int Coord -> [[Coord]]
nines m = foldr (\i cl -> nextPaths i cl m) (map return $ mLookup 0 m) (reverse [0..8])

main :: IO ()
main = print . length . nines . parseInput. lines =<< readFile "input/full.txt"