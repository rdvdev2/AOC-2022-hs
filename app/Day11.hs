module Main where

import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.List (transpose, sort)
import Data.Maybe

import Utils

newtype MonkeyId = MonkeyId Int deriving (Show)

type WorryLevel = Int
newtype Item = Item { getWorryLevel :: WorryLevel } deriving (Show)
newtype Items = Items { getItems :: [Item] } deriving (Show)

data Variable = Old | Constant WorryLevel deriving (Show)
data Operator = Plus | Times deriving (Show)
data Operation = Operation Variable Operator Variable deriving (Show)

newtype DivisibilityTest = DivisibilityTest WorryLevel deriving (Show)
newtype Target = Target MonkeyId deriving (Show)
type TrueTarget = Target
type FalseTarget = Target
data Test = Test DivisibilityTest TrueTarget FalseTarget deriving (Show)

newtype Unworrier = Unworrier (WorryLevel -> WorryLevel)

instance Show Unworrier where
    show _ = ""

data Monkey = Monkey MonkeyId Items Operation Test Unworrier deriving (Show)
newtype Monkeys = Monkeys { getMonkeys :: [Monkey] } deriving (Show)

instance InputParser MonkeyId where
    parseInput = MonkeyId . read . filter isDigit

instance InputParser Items where
    parseInput = Items . map (Item . read . filter isDigit) . drop 2 . words

instance InputParser Variable where
    parseInput "old" = Old
    parseInput st = Constant . read $ st

instance InputParser Operator where
    parseInput "+" = Plus
    parseInput "*" = Times
    parseInput _   = undefined

instance InputParser Operation where
    parseInput st = case words st of
        [_,_,_,x,op,y] -> Operation (parseInput x) (parseInput op) (parseInput y)
        _ -> undefined

instance InputParser DivisibilityTest where
    parseInput = DivisibilityTest . read . (!! 3) . words

instance InputParser Target where
    parseInput = Target . MonkeyId . read . (!! 5) . words

instance InputParser Test where
    parseInput st = case filter (not . null) . lines $ st of
        [divTest,trueTgt,falseTgt] -> Test (parseInput divTest) (parseInput trueTgt) (parseInput falseTgt)
        _ -> undefined

instance InputParser Monkey where
    parseInput st = case lines st of
        mid:items:op:test -> Monkey (parseInput mid) (parseInput items) (parseInput op) (parseInput . unlines $ test) (Unworrier id)
        _ -> undefined

instance InputParser Monkeys where
    parseInput st = case splitAt 7 . lines $ st of
        (x,[]) -> Monkeys [parseInput . unlines $ x]
        (x,xs) -> Monkeys ((parseInput . unlines $ x) : (getMonkeys . parseInput . unlines $ xs))

type ItemTransfer = (MonkeyId, Item)

asFunction :: Operator -> (WorryLevel -> WorryLevel -> WorryLevel)
asFunction Plus = (+)
asFunction Times = (*)

asValue :: Variable -> Item -> WorryLevel
asValue Old old = getWorryLevel old
asValue (Constant c) _ = c

performOperation :: Operation -> Unworrier -> Item -> Item
performOperation (Operation x op y) (Unworrier unw) i = let operator = asFunction op
                                                        in Item $ unw (asValue x i `operator` asValue y i)

performTest :: Test -> Item -> ItemTransfer
performTest (Test (DivisibilityTest divisor) (Target trueTgt) (Target falseTgt)) i
    | getWorryLevel i `mod` divisor == 0 = (trueTgt, i)
    | otherwise = (falseTgt, i)

dropItem :: State Monkey Item
dropItem = do Monkey mid (Items is) op test unw <- get
              put $ Monkey mid (Items . tail $ is) op test unw
              return $ head is

giveItem :: Item -> State Monkey ()
giveItem i = do Monkey mid (Items is) op test unw <- get
                put $ Monkey mid (Items (is ++ [i])) op test unw

inspectItem :: Monkey -> Item -> ItemTransfer
inspectItem (Monkey _ _ op test unw) = performTest test . performOperation op unw

inspectFirstItem :: State Monkey ItemTransfer
inspectFirstItem = do monkey <- get
                      inspectItem monkey <$> dropItem

itemCount :: Monkey -> Int
itemCount (Monkey _ (Items is) _ _ _) = length is

inspectAllItems :: State Monkey [ItemTransfer]
inspectAllItems = do monkey <- get
                     replicateM (itemCount monkey) inspectFirstItem

runMonkeyState :: State Monkey a -> MonkeyId -> State Monkeys a
runMonkeyState s (MonkeyId mid) = do
    monkeys <- get
    let maybeRunState idx monkey = if idx == mid
                                      then first Just . runState s $ monkey
                                      else (Nothing, monkey)
        (results, newMonkeys) = unzip . zipWith maybeRunState [0..] . getMonkeys $ monkeys
    put $ Monkeys newMonkeys
    return $ (head . catMaybes) results

inspectAllItems' :: MonkeyId -> State Monkeys [ItemTransfer]
inspectAllItems' = runMonkeyState inspectAllItems

passItem :: ItemTransfer -> State Monkeys ()
passItem (mid, i) = runMonkeyState (giveItem i) mid

passAllItems :: [ItemTransfer] -> State Monkeys ()
passAllItems = mapM_ passItem

inspectAndPass :: MonkeyId -> State Monkeys Int
inspectAndPass mid = do its <- inspectAllItems' mid
                        passAllItems its
                        return $ length its

itemPassingRound :: State Monkeys [Int]
itemPassingRound = do monkeys <- get
                      let monkeyCount = length . getMonkeys $ monkeys
                      mapM (inspectAndPass . MonkeyId) [0..monkeyCount-1]

itemPassingRounds :: Int -> State Monkeys [Int]
itemPassingRounds n = do counts <- replicateM n itemPassingRound
                         return $ map sum . transpose $ counts

monkeyBussinesLevel :: Int -> Monkeys -> Int
monkeyBussinesLevel n = product . take 2 . reverse . sort . evalState (itemPassingRounds n)

withUnworrier :: Unworrier -> Monkeys -> Monkeys
withUnworrier unw = Monkeys . map mappingFunction . getMonkeys
    where mappingFunction (Monkey mid is op test _) = Monkey mid is op test unw

getDivisor :: Monkey -> WorryLevel
getDivisor (Monkey _ _ _ (Test (DivisibilityTest n) _ _) _) = n

modUnworrier :: Monkeys -> Unworrier
modUnworrier = (\x -> Unworrier (`mod` x)) . product . map getDivisor . getMonkeys

main :: IO ()
main = solvePuzzle 11 solver

solver :: Monkeys -> (Int,Int)
solver m = ( monkeyBussinesLevel 20 . withUnworrier (Unworrier (`div` 3)) $ m
           , monkeyBussinesLevel 10000 . withUnworrier (modUnworrier m) $ m
           )