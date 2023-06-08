module Main where

import Data.Char
import Data.List

import Utils

data Crate = Crate Char deriving (Show)
newtype Stack = Stack { getCrates :: [Crate] } deriving (Show)
newtype Cargo = Cargo { getStacks :: [Stack] } deriving (Show)

data Instruction = Instruction { getAmmount :: Int, getOrigin :: Int, getDestination :: Int } deriving (Show)
newtype Instructions = Instructions { getInstructions :: [Instruction] } deriving (Show)

data Input = Input Cargo Instructions deriving (Show)

instance InputParser Stack where
    parseInput = Stack . (map Crate)

instance InputParser Cargo where
    parseInput st = let columns = transpose . lines $ st
                        filtered = filter (\x -> not ('[' `elem` x || ']' `elem` x || (words x) == [])) columns
                        normalized = map (filter isAlpha) filtered
                    in  Cargo . (map parseInput) $ normalized

instance InputParser Instruction where
    parseInput st = let st1  = drop 5 st
                        num1 = takeWhile (isDigit) st1
                        st2  = drop (length num1 + 6) st1
                        num2 = takeWhile (isDigit) st2
                        st3  = drop (length num2 + 4) st2
                        num3 = st3
                    in Instruction (read num1) (read num2) (read num3)

instance InputParser Instructions where
    parseInput = Instructions . map (parseInput) . lines

instance InputParser Input where
    parseInput st = case (break (=="") . lines) st of
                        (x,_:y) -> Input (parseLines x) (parseLines y)
                        _       -> undefined

main :: IO ()
main = solvePuzzle 5 solver

solver :: Input -> (String, String)
solver (Input cargo instructions) = ( map (\(Crate x) -> x) . cargoTop . performInstructions instructions $ cargo
                                    , map (\(Crate x) -> x) . cargoTop . performInstructions' instructions $ cargo
                                    )

pop :: Stack -> (Stack, Crate)
pop (Stack (x:xs)) = (Stack xs, x)
pop (Stack []) = undefined

push :: Crate -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

popGroup :: Int -> Stack -> (Stack, Stack)
popGroup n (Stack st) = (Stack . drop n $ st, Stack . take n $ st)

pushGroup :: Stack -> Stack -> Stack
pushGroup (Stack gp) (Stack st) = Stack (gp ++ st)

popFrom :: Int -> Cargo -> (Cargo, Crate)
popFrom 1 (Cargo (x:xs)) = let (newx, popped) = pop x
                           in  (Cargo (newx:xs), popped)
popFrom st (Cargo (x:xs)) = let (newxs, popped) = popFrom (st-1) (Cargo xs)
                            in  (Cargo (x : getStacks newxs), popped)
popFrom _ (Cargo []) = undefined

pushTo :: Int -> Crate -> Cargo -> Cargo
pushTo 1 crate (Cargo (x:xs)) = Cargo (push crate x : xs)
pushTo st crate (Cargo (x:xs)) = Cargo (x : getStacks (pushTo (st-1) crate (Cargo xs)))
pushTo _ _ (Cargo []) = undefined

popGroupFrom :: Int -> Int -> Cargo -> (Cargo, Stack)
popGroupFrom 1 n (Cargo (x:xs)) = let (newx, popped) = popGroup n x
                                  in  (Cargo (newx:xs), popped)
popGroupFrom st n (Cargo (x:xs)) = let (newxs, popped) = popGroupFrom (st-1) n (Cargo xs)
                                   in  (Cargo (x : getStacks newxs), popped)
popGroupFrom _ _ (Cargo []) = undefined

pushGroupTo :: Int -> Stack -> Cargo -> Cargo
pushGroupTo 1 stack (Cargo (x:xs)) = Cargo (pushGroup stack x : xs)
pushGroupTo st stack (Cargo (x:xs)) = Cargo (x : getStacks (pushGroupTo (st-1) stack (Cargo xs)))
pushGroupTo _ _ (Cargo []) = undefined

moveCrate :: Int -> Int -> Cargo -> Cargo
moveCrate orig dest cargo = let (afterPop, x) = popFrom orig cargo
                            in  pushTo dest x afterPop

moveCrateGroup :: Int -> Int -> Int -> Cargo -> Cargo
moveCrateGroup orig dest n cargo = let (afterPop, x) = popGroupFrom orig n cargo
                                   in  pushGroupTo dest x afterPop

performInstruction :: Instruction -> Cargo -> Cargo
performInstruction (Instruction c orig dest) cargo = iterate (moveCrate orig dest) cargo !! c

performInstruction' :: Instruction -> Cargo -> Cargo
performInstruction' (Instruction c orig dest) cargo = moveCrateGroup orig dest c cargo

performInstructions :: Instructions -> Cargo -> Cargo
performInstructions instructions cargo = foldl (flip performInstruction) cargo . getInstructions $ instructions

performInstructions' :: Instructions -> Cargo -> Cargo
performInstructions' instructions cargo = foldl (flip performInstruction') cargo . getInstructions $ instructions

cargoTop :: Cargo -> [Crate]
cargoTop = map (head . getCrates) . getStacks
