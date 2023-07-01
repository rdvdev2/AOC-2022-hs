module Main where

import Test.HUnit

import TestUtils
import Utils

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [ TestLabel "Day 1" day1
                 , TestLabel "Day 2" day2
                 , TestLabel "Day 3" day3
                 , TestLabel "Day 4" day4
                 , TestLabel "Day 5" day5
                 , TestLabel "Day 6" day6
                 , TestLabel "Day 7" day7
                 , TestLabel "Day 8" day8
                 , TestLabel "Day 9" day9
                 , TestLabel "Day 10" day10
                 , TestLabel "Day 11" day11
                 ]

day1 :: Test
day1 = TestCase $ assertComputedSolutionsEqual 1 67622 201491

day2 :: Test
day2 = TestCase $ assertComputedSolutionsEqual 2 13526 14204

day3 :: Test
day3 = TestCase $ assertComputedSolutionsEqual 3 8349 2681

day4 :: Test
day4 = TestCase $ assertComputedSolutionsEqual 4 515 883

day5 :: Test
day5 = TestCase $ assertComputedSolutionsEqual 5 "VQZNJMWTR" "NLCDCLVMQ"

day6 :: Test
day6 = TestCase $ assertComputedSolutionsEqual 6 1651 3837

day7 :: Test
day7 = TestCase $ assertComputedSolutionsEqual 7 1297159 3866390

day8 :: Test
day8 = TestCase $ assertComputedSolutionsEqual 8 1719 590824

day9 :: Test
day9 = TestCase $ assertComputedSolutionsEqual 9 5779 2331

day10 :: Test
day10 = TestCase $ assertComputedSolutionsEqual 10 14360 (MultilineString
    [ "###...##..#..#..##..####.###..####.####."
    , "#..#.#..#.#.#..#..#.#....#..#.#.......#."
    , "###..#....##...#..#.###..#..#.###....#.."
    , "#..#.#.##.#.#..####.#....###..#.....#..."
    , "#..#.#..#.#.#..#..#.#....#.#..#....#...."
    , "###...###.#..#.#..#.####.#..#.####.####."
    ])

day11 :: Test
day11 = TestCase $ assertComputedSolutionsEqual 11 67830 15305381442