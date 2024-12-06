module Main where

import Debug.Trace (trace)
import Parser
import GHC.Base

dPrint :: Show a => a -> a
dPrint x = trace (show x) x

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
allReadings s = map (\f -> f s) [toULBR, toBRUL, toURBL, toBLUR]

parseMASOneDirection :: [String] -> Int
parseMASOneDirection = sum . map (length . consumeRawUnsafe (many $ discardToParse $ parseStr "MAS"))

main :: IO () -- todo : supprimer ceux qui font pas croix et / par 2
main = print . sum . map parseMASOneDirection . allReadings . lines =<< readFile "input/test.txt"