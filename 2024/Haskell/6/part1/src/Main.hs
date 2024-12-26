module Main where

import Data.List
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

-----------

data Dir = U | R | D | L
  deriving (Show)

instance Enum Dir where
  toEnum n = case n `mod` 4 of
    0 -> U
    1 -> R
    2 -> D
    3 -> L

  fromEnum x = case x of
    U -> 0
    R -> 1
    D -> 2
    L -> 3

type Coord = (Int, Int)

toCoord :: Dir -> Coord
toCoord d = case d of
  U -> (-1, 0)
  R -> (0, 1)
  D -> (1, 0)
  L -> (0, -1)

add :: Coord -> Coord -> Coord
add (a, b) (c, d) = (a + c, b + d)

data State = State
  { plan :: [[Char]],
    guardCoord :: Coord,
    guardDir :: Dir
  }
  deriving (Show)

fromInput :: [[Char]] -> State
fromInput l =
  let [[(i, j, _)]] = filter (/= []) (zipWith (\i s -> filter (\(_, _, c) -> c == '^') (zipWith (\j c -> (i, j, c)) [0 ..] s)) [0 ..] l)
   in State {plan = l, guardCoord = (i, j), guardDir = U}

allState =
  unfoldr
    ( \s ->
        let State {plan = p, guardCoord = gc, guardDir = d} = s
         in let ngc = add gc (toCoord d)
             in case (p !?? ngc, p !?? gc) of
                  (Just '#', _) -> Just (s, s {guardDir = succ d})
                  (Nothing, Nothing) -> Nothing  
                  _ -> Just (s, s {plan = arr2Dassign p ngc 'X', guardCoord = ngc})
    )

-----------

(!??) :: [[a]] -> (Int, Int) -> Maybe a
l !?? (i, j)
  | i < 0 || j < 0 = Nothing
  | otherwise = (listToMaybe . drop i) l >>= (listToMaybe . drop j)

arr2Dassign :: [[a]] -> (Int, Int) -> a -> [[a]]
arr2Dassign l (x, y) e = [[if x == i && y == j then e else f | (j, f) <- zip [0 ..] s] | (i, s) <- zip [0 ..] l]

main :: IO ()
main = print . sum . map (length . filter (\c -> c == 'X' || c == '^')) . plan . last . allState . fromInput . lines =<< readFile "input/full.txt"