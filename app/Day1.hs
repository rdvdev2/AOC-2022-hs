module Main where

import Data.List ( sortBy )
import Data.List.Split

import Utils

type Calories = Int
type Elf = [Calories]
newtype Elves = Elves { toList :: [Elf] }

instance InputParser Elves where
   parseInput inp = let calories = lines inp
                        rawElves = splitOn [""] calories
                    in Elves $ map (map read) rawElves

main :: IO ()
main = solvePuzzle 1 solver

solver :: Elves -> (Calories,Calories)
solver elves = let totals = map sum (toList elves)
                   orderedTotals = sortBy (flip compare) totals
               in ( head orderedTotals
                  , (sum . take 3) orderedTotals
                  )
