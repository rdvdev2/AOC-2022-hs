module TestUtils ( assertComputedSolutionsEqual ) where

import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import System.Process.Typed
import Test.HUnit

-- Biggest hack in my life, probably horrible
toString :: ByteString -> String
toString = read . show

filterSolutions :: String -> (String, String)
filterSolutions st =
    let outputLines = tail . lines $ st
    in  case break ("Part 2:" `isPrefixOf`) outputLines of
            (p1, p2) -> (filterSolution p1, filterSolution p2)
        where filterSolution = drop 8 . unlines

computedSolutions :: Int -> IO (String, String)
computedSolutions n = do
    let procConfig = proc ("day" ++ show n) []
    output <- readProcessStdout_ procConfig
    let outputString = toString output
    return (filterSolutions outputString)

assertComputedSolutionsEqual :: (Show a, Show b) => Int -> a -> b -> Assertion
assertComputedSolutionsEqual n p1 p2 = do
    (part1, part2) <- computedSolutions n
    assertEqual "for part 1," (show p1 ++ "\n") part1
    assertEqual "for part 2," (show p2 ++ "\n") part2
