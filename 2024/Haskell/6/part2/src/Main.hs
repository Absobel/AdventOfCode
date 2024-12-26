module Main where

import Data.List
import Data.Maybe (listToMaybe)
import qualified Data.Maybe as Maybe
import Debug.Trace (trace)

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

-----------

data Dir = U | R | D | L
  deriving (Show, Eq)

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

sub :: Coord -> Coord -> Coord
sub (a, b) (c, d) = (a - c, b - d)

data State = State
  { walls :: [Coord],
    guardPath :: [(Coord, Dir)],
    size :: (Int, Int)
  }
  deriving (Show)

fromInput :: [[Char]] -> State
fromInput l =
  let wpp = concat $ filter (/= []) (zipWith (\i s -> filter (\(_, _, c) -> c /= '.') (zipWith (\j c -> (i, j, c)) [0 ..] s)) [0 ..] l)
   in let wp = [(i, j) | (i, j, c) <- wpp, c == '#']
       in let gc = [((i, j), U) | (i, j, c) <- wpp, c == '^']
           in let (mx, my) = (length l, length $ head l)
               in State {walls = wp, guardPath = gc, size = (mx, my)}

allState =
  unfoldr
    ( \s ->
        let State {walls = w, guardPath = p, size = (mx, my)} = s
         in let ((gx, gy), d) = head p
             in let (ngc, nd) = case d of
                      U -> case (listToMaybe (sortOn (\(wx, wy) -> -wx) $ filter (\(wx, wy) -> wy == gy && wx < gx) w), R) of (Just ngc, nd) -> (add (1, 0) ngc, nd); (Nothing, _) -> ((-1, -1), U)
                      R -> case (listToMaybe (sortOn snd $ filter (\(wx, wy) -> wy > gy && wx == gx) w), D) of (Just ngc, nd) -> (add (0, -1) ngc, nd); (Nothing, _) -> ((-1, -1), R)
                      D -> case (listToMaybe (sortOn fst $ filter (\(wx, wy) -> wy == gy && wx > gx) w), L) of (Just ngc, nd) -> (add (-1, 0) ngc, nd); (Nothing, _) -> ((-1, -1), D)
                      L -> case (listToMaybe (sortOn (\(wx, wy) -> -wy) $ filter (\(wx, wy) -> wy < gy && wx == gx) w), U) of (Just ngc, nd) -> (add (0, 1) ngc, nd); (Nothing, _) -> ((-1, -1), L)
                 in if ((gx, gy), d) `elem` tail p then Nothing else Just (s, s {guardPath = (ngc, nd) : p})
    )

-- to add the last remaining bit of the way til the border
pushToLimit s = let (sx, sy) = size s in let gc = guardPath s in let ((gx, gy), d) = head gc in let (nx, ny) = case d of U -> (0, gy); R -> (gx, sy - 1); D -> (sx - 1, gy); L -> (gx, 0) in s {guardPath = ((nx, ny), d) : gc}

purify s = let gc = guardPath s in let (sx, sy) = size s in s {guardPath = filter (\((i,j),_) -> i >= 0 && i < sx && j >= 0 && j < sy) gc}

allWallToTest s = removeDupes $ interpol $ init $ guardPath s

interpol :: [(Coord, Dir)] -> [Coord]
interpol [(x, d)] = [x]
interpol (((x1, y1), _) : ((x2, y2), d) : q) = init [(x, y) | x <- rangeBothSide x1 x2, y <- rangeBothSide y1 y2] ++ interpol (((x2, y2), d) : q)

hasLooped :: State -> Bool
hasLooped s = let (n,_) = head (guardPath s) in n /= (-1,-1) 

fullLoopTest inS = let refS = pushToLimit $ purify $ last $ allState inS in
  let inW = walls inS in
  length $ filter id $ map (\w -> hasLooped $ last $ allState (inS {walls = w:inW})) (allWallToTest refS)

-----------

(!??) :: [[a]] -> (Int, Int) -> Maybe a
l !?? (i, j)
  | i < 0 || j < 0 = Nothing
  | otherwise = (listToMaybe . drop i) l >>= (listToMaybe . drop j)

arr2Dassign :: [[a]] -> (Int, Int) -> a -> [[a]]
arr2Dassign l (x, y) e = [[if x == i && y == j then e else f | (j, f) <- zip [0 ..] s] | (i, s) <- zip [0 ..] l]

rangeBothSide :: Int -> Int -> [Int]
rangeBothSide a b
  | a <= b = [a .. b]
  | otherwise = reverse [b .. a]

removeDupes :: Eq a => [a] -> [a]
removeDupes [] = []
removeDupes (t:q) = let rq = removeDupes q in if t `elem` rq then rq else t:rq

main :: IO ()
main = print . fullLoopTest. fromInput . lines =<< readFile "input/full.txt"