module Utils ( InputParser(parseInput), solvePuzzle, solvePuzzle' ) where

class InputParser a where
    parseInput :: String -> a

instance (InputParser a, InputParser b) => InputParser (a,b) where
    parseInput st = (parseInput st, parseInput st)

getInput :: Int -> IO String
getInput day = readFile $ "inputs/day" ++ show day ++ ".inp"

getParsedInput :: (InputParser a) => Int -> IO a
getParsedInput day = do
    input <- getInput day
    return $ parseInput input

displayPartAnswer :: (Show a) => Int -> a -> IO ()
displayPartAnswer n answer = putStrLn $ "Part " ++ show n ++ ": " ++ show answer

displayPuzzleAnswers :: (Show a, Show b) => a -> b -> IO ()
displayPuzzleAnswers part1 part2 = do
    displayPartAnswer 1 part1
    displayPartAnswer 2 part2

solvePuzzle :: (InputParser a, Show c, Show d) => Int -> (a -> (c,d)) -> IO ()
solvePuzzle n solver = do
    input <- getParsedInput n
    let (part1,part2) = solver input
    putStrLn $ "== Day " ++ show n ++ " answers =="
    displayPuzzleAnswers part1 part2

solvePuzzle' :: (InputParser a, InputParser b, Show c, Show d) => Int -> (a -> b -> (c,d)) -> IO ()
solvePuzzle' n solver = solvePuzzle n (\(i1,i2) -> solver i1 i2)