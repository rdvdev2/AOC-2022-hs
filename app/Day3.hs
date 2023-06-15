module Main where

import Data.Char
import Data.List

import Utils

type Item = Char
type Compartment = [Item]
data Rucksack = Rucksack Compartment Compartment
newtype Rucksacks = Rucksacks { toRucksackList :: [Rucksack] }
type Priority = Int
type ElveGroup = (Rucksack,Rucksack,Rucksack)

instance InputParser Rucksack where
    parseInput st = Rucksack (take l st) (drop l st)
        where l = (length st) `div` 2

instance InputParser Rucksacks where
    parseInput = Rucksacks . (map parseInput)  . lines

main :: IO ()
main = solvePuzzle 3 solver

solver :: Rucksacks -> (Priority,Priority)
solver rucksacks = ( sum . map (itemPriority . sharedItem) $ toRucksackList rucksacks
                   , sum . map (itemPriority . badge) . elveGroups $ rucksacks
                   )

sharedItem :: Rucksack -> Item
sharedItem (Rucksack c1 c2) = head (intersect c1 c2)

itemPriority :: Item -> Priority
itemPriority x
    | isLower x = ord x - ord 'a' + 1
    | isUpper x = ord x - ord 'A' + 27
    | otherwise     = 0

elveGroups :: Rucksacks -> [ElveGroup]
elveGroups (Rucksacks []) = []
elveGroups (Rucksacks (x:y:z:xs)) = (x,y,z) : elveGroups (Rucksacks xs)
elveGroups _ = undefined

allItems :: Rucksack -> [Item]
allItems (Rucksack c1 c2) = union c1 c2

badge :: ElveGroup -> Item
badge (x,y,z) = let x' = allItems x
                    y' = allItems y
                    z' = allItems z
                in  head $ intersect x' (intersect y' z')
