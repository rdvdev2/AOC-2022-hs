module Main where

import Data.ByteString.Lazy (ByteString)
import System.Process.Typed
import Control.Monad
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
    putStrLn ""
    let totalTests = Prelude.length expectedSolutions
    results <- forM [1..totalTests] checkSolver
    let passedTests = length . filter id $ results
    putStrLn ""
    if passedTests == totalTests
        then do
            putStrLn $ "Everything alright: All " ++ show totalTests ++ " tests passed! 🚀"
            exitSuccess
        else do
            putStrLn $ "We're doomed: Only " ++ show passedTests ++ " out of " ++ show totalTests ++ " tests passed! 💩"
            exitFailure

-- Biggest hack in my life, probably horrible
toString :: ByteString -> String
toString = read . show

filterSolutions :: String -> (String, String)
filterSolutions st = let outputLines = lines st
                     in  ( Prelude.drop 8 . (!! 1) $ outputLines
                         , Prelude.drop 8 . (!! 2) $ outputLines
                         ) 

runSolver :: Int -> IO (String, String)
runSolver n = do
    let procConfig = proc ("day" ++ show n) []
    output <- readProcessStdout_ procConfig
    let outputString = toString output
    return (filterSolutions outputString)

checkSolver :: Int -> IO Bool
checkSolver n = do
    solution <- runSolver n
    let expectedSolution = expectedSolutions !! (n - 1)
        pass             = solution == expectedSolution
    putStrLn $ "Day " ++ show n ++ ": "  ++ (if pass then "PASS ✔️" else "FAIL ❌")
    unless pass (
        putStrLn $ "  Expected solution: " ++ show expectedSolution ++
                 "\n  Outputed solution: " ++ show solution
        )
    return pass


------------------
-- TESTING DATA --
------------------

expectedSolutions :: [(String, String)]
expectedSolutions = [ ("67622", "201491")
                    , ("13526", "14204")
                    , ("8349", "2681")
                    , ("515", "883")
                    , ("\"VQZNJMWTR\"", "\"NLCDCLVMQ\"")
                    , ("1651", "3837")
                    , ("1297159", "3866390")
                    , ("1719", "590824")
                    , ("5779", "2331")
                    ]