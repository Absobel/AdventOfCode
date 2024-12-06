module Main where

import Debug.Trace (trace)
import Parser
import GHC.Base

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

-- length String == length [String] sinon marche pas
kindaZip :: String -> [String] -> [String]
kindaZip "" [] = []
kindaZip s [] = kindaZip s (map (const []) s)
kindaZip (c : s) (t : q) = (c : t) : kindaZip s q

toLR :: [String] -> [String]
toLR = id
toRL :: [String] -> [String]
toRL = map reverse . toLR
toUB :: [String] -> [String]
toUB = Prelude.foldr kindaZip []
toBU :: [String] -> [String]
toBU = map reverse . toUB

toBRUL :: [String] -> [String]
toBRUL = map reverse . toULBR
toULBR :: [String] -> [String]
toULBR = toBLUR . reverse
toURBL :: [String] -> [String]
toURBL = map reverse . toBLUR
toBLUR :: [String] -> [String]
toBLUR [] = []
toBLUR ([] : xss) = xss
toBLUR xss =
  zipWith
    (++)
    (map ((: []) . head) xss ++ repeat [])
    ([] : toBLUR (map tail xss))

allReadings :: [String] -> [[String]]
allReadings s = map (\f -> f s) [toLR, toRL, toUB, toBU, toULBR, toBRUL, toURBL, toBLUR]

parseXMASOneDirection :: [String] -> Int
parseXMASOneDirection = sum . map (length . consumeRawUnsafe (many $ discardToParse $ parseStr "XMAS"))

main :: IO ()
main = print . sum . map parseXMASOneDirection . allReadings . lines =<< readFile "input/full.txt"