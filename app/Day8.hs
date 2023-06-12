module Main where

import Data.Char (digitToInt)

import Utils

type Height = Int;
newtype Map = Map { getHeights :: [[Int]] } deriving (Show)

instance InputParser Map where
    parseInput = Map . map (map digitToInt) . lines

main :: IO ()
main = solvePuzzle 8 solver

solver :: Map -> (Int, Int)
solver m = ( length . filter (flip posVisible m) . getPositions $ m
           , maximum . map (flip scenicScore m) . getPositions $ m
           )

type Pos = (Int, Int)
type Path = [Height]
data Direction = DLeft | DRight | DUp | DDown

getPos :: Pos -> Map -> Height
getPos (x,y) = (!! x) . (!! y) . getHeights

move :: Direction -> Pos -> Pos
move DLeft  (x,y) = (x-1,y)
move DRight (x,y) = (x+1,y)
move DUp    (x,y) = (x,y+1)
move DDown  (x,y) = (x,y-1)

getDimensions :: Map -> (Int,Int)
getDimensions (Map m) = (length $ m !! 0, length m)

getPositions :: Map -> [Pos]
getPositions m = let (width,height) = getDimensions m
                 in  [(x,y) | x <- [0..width-1], y <- [0..height-1]]

isValid :: Pos -> Map -> Bool
isValid (x,y) m = let (width,height) = getDimensions m
                  in  x >= 0 && y >= 0 && x < width && y < height

tryMove :: Direction -> Pos -> Map -> Maybe Pos
tryMove dir pos m = let newPos = move dir pos
                    in  if isValid newPos m
                            then Just newPos
                            else Nothing

path :: Direction -> Pos -> Map -> Path
path dir pos m = case tryMove dir pos m of
                     Nothing     -> [getPos pos m]
                     Just newPos -> [getPos pos m] ++ (path dir newPos m)

paths :: Pos -> Map -> [Path]
paths pos m = map (\dir -> path dir pos m) [DLeft,DRight,DUp,DDown]

pathVisible :: Path -> Bool
pathVisible (x:xs) = all (<x) xs
pathVisible [] = undefined

posVisible :: Pos -> Map -> Bool
posVisible pos = any pathVisible . paths pos

visibleTreesInPath :: Path -> Int
visibleTreesInPath (x:xs) = let (visible,others) = span (<x) xs
                            in  if length others == 0
                                    then length visible
                                    else length visible + 1
visibleTreesInPath [] = undefined

scenicScore :: Pos -> Map -> Int
scenicScore pos = product . map visibleTreesInPath . paths pos