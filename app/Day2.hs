module Main where

import Utils

data Play = Rock | Paper | Scissors deriving (Show,Eq)
type Round = (Play, Play)
newtype Game = Game { toRounds :: [Round] }
data RoundOutcome = Lose | Draw | Win
type RoundStrategy = (Play, RoundOutcome)
newtype Strategy = Strategy { toRoundStrategies :: [RoundStrategy] }

instance InputParser Game where
    parseInput = Game . map parseRound . lines

instance InputParser Strategy where
    parseInput = Strategy . map parseRoundStrategy . lines

main :: IO ()
main = solvePuzzle' 2 solver

solver :: Game -> Strategy -> (Points,Points)
solver game strat = ( (sum . map roundPoints) $ toRounds game
                    , (sum . map roundStrategyPoints) $ toRoundStrategies strat
                    )

charToPlay :: Char -> Play
charToPlay x
    | x == 'A' || x == 'X' = Rock
    | x == 'B' || x == 'Y' = Paper
    | x == 'C' || x == 'Z' = Scissors
    | otherwise            = undefined

charToOutcome :: Char -> RoundOutcome
charToOutcome x
    | x == 'X'  = Lose
    | x == 'Y'  = Draw
    | x == 'Z'  = Win
    | otherwise = undefined

parseRound :: String -> Round
parseRound [x, ' ', y] = (charToPlay x, charToPlay y)
parseRound _ = undefined

parseRoundStrategy :: String -> RoundStrategy
parseRoundStrategy [x, ' ', y] = (charToPlay x, charToOutcome y)
parseRoundStrategy _ = undefined

invert :: RoundOutcome -> RoundOutcome
invert Lose = Win
invert Draw = Draw
invert Win  = Lose

roundOutcome :: Round -> RoundOutcome
roundOutcome (Rock,Paper)     = Win
roundOutcome (Paper,Scissors) = Win
roundOutcome (Scissors,Rock)  = Win
roundOutcome (x,y)
    | x == y    = Draw
    | otherwise = invert (roundOutcome (y,x))

shouldPlay :: RoundStrategy -> Play
shouldPlay (Rock,Win)      = Paper
shouldPlay (Paper,Win)     = Scissors
shouldPlay (Scissors,Win)  = Rock
shouldPlay (Rock,Lose)     = Scissors
shouldPlay (Paper,Lose)    = Rock
shouldPlay (Scissors,Lose) = Paper
shouldPlay (x,Draw)        = x

type Points = Int

playPoints :: Play -> Points
playPoints Rock     = 1
playPoints Paper    = 2
playPoints Scissors = 3

outcomePoints :: RoundOutcome -> Points
outcomePoints Lose = 0
outcomePoints Draw = 3
outcomePoints Win  = 6

roundPoints :: Round -> Points
roundPoints r@(_,p) = playPoints p + outcomePoints (roundOutcome r)

roundStrategyPoints :: RoundStrategy -> Points
roundStrategyPoints rs@(_,o) = playPoints (shouldPlay rs) + outcomePoints o