{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module Main where

import Debug.Trace (trace, traceShow)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Maybe
import Data.List

dPrint :: Show a => (b -> a) -> b -> b
dPrint f x = trace (show $ f x) x

----------

type MultiMap k v = Map k [v]

mLookup :: Ord k => k -> MultiMap k v -> [v]
mLookup k m = Maybe.fromMaybe [] (Map.lookup k m)

mInsert :: Ord k => k -> a -> MultiMap k a -> MultiMap k a
mInsert k v = Map.insertWith (++) k [v]

mInsertWith :: Ord k => (a -> [a] -> [a]) -> k -> a -> MultiMap k a -> MultiMap k a
mInsertWith f k v = Map.insertWith (\[a1] a2 -> f a1 a2) k [v]

mFromList :: Ord k => [(k, v)] -> MultiMap k v
mFromList = foldr (\(k, v) acc -> mInsert k v acc) Map.empty

----------

uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

type Coord = (Int, Int)

parseInput :: [[Char]] -> [[Coord]]
parseInput l = concatMap snd $ Map.toList $ foldr (\(i, s) m1 -> foldr (\(j, c) m2 -> if any ((i,j) `elem`) (mLookup c m2) then m2 else mInsert c (parcel c (i,j) l) m2) m1 (zip [0..] s)) Map.empty (zip [0..] l)

(!!!) :: [a] -> Int -> Maybe a
xs !!! i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

arr :: Coord -> [[a]] -> Maybe a
arr (x,y) l = (!!! y) =<< (l !!! x)

validarr :: (a -> Bool) -> Coord -> [[a]] -> Maybe a
validarr f xy l = arr xy l >>= \x -> if f x then Just x else Nothing

parcel :: Char -> Coord -> [[Char]] -> [Coord]
parcel c xy l = parcelAux c xy l [xy]
-- why y'a besoin d'uniq ?? me dépasse
parcelAux c xy l xys = uniq $ let vn = filter (\coo -> coo `notElem` xys && isJust (validarr (== c) coo l)) (neighbors xy) in foldr (\nxy acc -> acc ++ parcelAux c nxy l acc) (xys ++ vn) vn

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

price :: [Coord] -> [[Char]]  -> Int
price t l = let c = fromJust $ arr (head t) l in let (p, a) = foldr (\xy (pacc, aacc) -> let lp = length $ filter (\nxy -> isNothing (validarr (== c) nxy l)) (neighbors xy) in (pacc+lp, aacc+1)) (0, 0) t in p*a

main :: IO ()
main = print . (\l -> foldr (\ml acc -> acc + price ml l) 0 $ parseInput l) . lines =<< readFile "input/full.txt"