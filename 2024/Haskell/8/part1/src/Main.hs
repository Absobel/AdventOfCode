module Main where

import Debug.Trace (trace)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.List

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

----------------

type Bound = (Int, Int)
type Coord = (Int, Int)

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

----------------

-- return size and usable input (size, list antennas)
parseInput :: [[Char]] -> (Bound, [[Coord]])
parseInput l = let s = (length $ head l, length l) in (s, map snd $ Map.toList $ foldr (\(k, v) acc -> Map.insertWith (++) k [v] acc) Map.empty $ concat $ zipWith (\i s -> filter (\(c, _) -> c /= '.') $ zipWith (\j c -> (c, (i,j))) [0..] s) [0..] l)

antiNodes :: Bound -> [Coord] -> [Coord]
antiNodes s l = concat [ filter (isInBounds s) [(2*ax2 - ax1, 2*ay2 - ay1), (2*ax1 - ax2, 2*ay1 - ay2)] | (i, (ax1, ay1)) <- zip [0..] l, (j,(ax2, ay2)) <- zip [0..] l, i < j]

isInBounds :: Bound -> Coord -> Bool
isInBounds (mx, my) (x, y) = 0 <= x && x < mx && 0 <= y && y < my

main :: IO ()
main = print . length . (\(s, la) -> uniq $ concatMap (antiNodes s) la) . parseInput . lines =<< readFile "input/full.txt"