module Main where

import Control.Monad.Writer
import Data.Function
import Data.List ( groupBy )

import Utils

data Instruction = Addx Int | Noop deriving (Show)
newtype Program = Program { getInstructions :: [Instruction] } deriving (Show)

instance InputParser Instruction where
    parseInput st = case opcode of
        "addx" -> Addx (read parameter)
        "noop" -> Noop
        _ -> undefined
        where
            opcode = head . words $ st
            parameter = words st !! 1

instance InputParser Program where
    parseInput = Program . map parseInput . lines

data CPU = CPU { programCounter :: Int, registerX :: Int }

elapsedCycles :: CPU -> CPU -> Int
elapsedCycles x y = programCounter y - programCounter x

incrementProgramCounter :: Int -> CPU -> CPU
incrementProgramCounter inc (CPU pc x) = CPU (pc+inc) x

addToRegisterX :: Int -> CPU -> CPU
addToRegisterX inc (CPU pc x) = CPU pc (x+inc)

runInstruction :: Instruction -> CPU -> CPU
runInstruction (Addx dx) = incrementProgramCounter 2 . addToRegisterX dx
runInstruction Noop      = incrementProgramCounter 1

runInstructionTracked :: Instruction -> CPU -> Writer [Int] CPU
runInstructionTracked instr cpu = do
    let result = runInstruction instr cpu
        cycles = elapsedCycles cpu result
    replicateM_ cycles $ tell [registerX cpu]
    return result

runProgram :: Program -> Writer [Int] ()
runProgram (Program instrs) = do
    let initialState = CPU 0 1
    foldM_ (flip runInstructionTracked) initialState instrs

registerXHistory :: Program -> [(Int,Int)]
registerXHistory = zip [1..]  . snd . runWriter . runProgram

totalSignalStrength :: [(Int,Int)] -> Int
totalSignalStrength hist = sum . map (uncurry (*)) $ interestingSignals
    where interestingSignals = filter (\x -> fst x `elem` interestingCycles) hist
          interestingCycles  = [20,60,100,140,180,220]

generateScreenImage :: [(Int, Int)] -> MultilineString
generateScreenImage hist = MultilineString . map raster $ rows
    where rows = groupBy ((==) `on` (\(idx,_) -> (idx - 1) `div` 40)) hist
          raster = map (\(idx,x) -> if abs (((idx - 1) `mod` 40) - x) <= 1 then '#' else '.')

main :: IO ()
main = solvePuzzle 10 solver

solver :: Program -> (Int, MultilineString)
solver p = ( totalSignalStrength . registerXHistory $ p
           , generateScreenImage . registerXHistory $ p
           )