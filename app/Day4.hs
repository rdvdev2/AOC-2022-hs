module Main where

import Data.List

import Utils

data Assignment = Assignment Int Int deriving (Show)
data ElfPair = ElfPair Assignment Assignment deriving (Show)
newtype ElfPairs = ElfPairs { getPairs :: [ElfPair] } deriving (Show)

instance InputParser Assignment where
    parseInput st = case break (=='-') st of
                        (x,_:y) -> Assignment (read x) (read y)
                        _       -> undefined

instance InputParser ElfPair where
    parseInput st = case break (==',') st of
                        (x,_:y) -> ElfPair (parseInput x) (parseInput y)
                        _       -> undefined

instance InputParser ElfPairs where
    parseInput = (ElfPairs . map (parseInput) . lines)

main :: IO ()
main = solvePuzzle 4 solver

solver :: ElfPairs -> (Int, Int)
solver elfPairs = ( length . filter (fullOverlap) . getPairs $ elfPairs
                  , length . filter (someOverlap) . getPairs $ elfPairs
                  )

fullyOverlaps :: Assignment -> Assignment -> Bool
fullyOverlaps (Assignment s1 e1) (Assignment s2 e2) = (s1 <= s2) && (e1 >= e2)

fullOverlap :: ElfPair -> Bool
fullOverlap (ElfPair x y) = fullyOverlaps x y || fullyOverlaps y x

someOverlap :: ElfPair -> Bool
someOverlap (ElfPair (Assignment s1 e1) (Assignment s2 e2)) = length intersection > 0
    where intersection = intersect [s1..e1] [s2..e2]