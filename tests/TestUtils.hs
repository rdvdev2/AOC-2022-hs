module TestUtils ( assertComputedSolutionsEqual ) where

import Data.ByteString.Lazy (ByteString)
import System.Process.Typed
import Test.HUnit

-- Biggest hack in my life, probably horrible
toString :: ByteString -> String
toString = read . show

filterSolutions :: String -> (String, String)
filterSolutions st = let outputLines = lines st
                     in  ( Prelude.drop 8 . (!! 1) $ outputLines
                         , Prelude.drop 8 . (!! 2) $ outputLines
                         ) 

computedSolutions :: Int -> IO (String, String)
computedSolutions n = do
    let procConfig = proc ("day" ++ show n) []
    output <- readProcessStdout_ procConfig
    let outputString = toString output
    return (filterSolutions outputString)

assertComputedSolutionsEqual :: Show a => Int -> a -> a -> Assertion
assertComputedSolutionsEqual n p1 p2 = do
    (part1, part2) <- computedSolutions n
    assertEqual "for part 1," part1 (show p1)
    assertEqual "for part 2," part2 (show p2)
