module Main where

import Data.List ( nub )

import Utils

newtype DatastreamBuffer = DatastreamBuffer { getData :: String }

instance InputParser DatastreamBuffer where
    parseInput = DatastreamBuffer

main :: IO ()
main = solvePuzzle 6 solver

solver :: DatastreamBuffer -> (Int, Int)
solver buf = ( startOfPacket buf
             , startOfMessage buf
             )

packetTail :: DatastreamBuffer -> DatastreamBuffer
packetTail (DatastreamBuffer x) = DatastreamBuffer (tail x)

distinct :: (Eq a) => [a] -> Int
distinct = length . nub

distinctHeadItems :: (Eq a) => Int -> [a] -> Int
distinctHeadItems n = distinct . take n

allHeadItemsDistinct :: (Eq a) => Int -> [a] -> Bool
allHeadItemsDistinct n = (== n) . distinctHeadItems n

endOfFirstDistinctChain :: (Eq a) => Int -> [a] -> Int
endOfFirstDistinctChain n = (+n) . length . takeWhile not . map (allHeadItemsDistinct n) . iterate tail

startOfPacket :: DatastreamBuffer -> Int
startOfPacket = endOfFirstDistinctChain 4 . getData

startOfMessage :: DatastreamBuffer -> Int
startOfMessage = endOfFirstDistinctChain 14 . getData