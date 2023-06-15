module Main where

import Control.Monad.Writer
import Data.List (nub)

import Utils

data Direction = DLeft | DRight | DUp | DDown deriving (Show)
type Steps = Int
data Command = Command Direction Steps deriving (Show)
newtype Commands = Commands { getCommands :: [Command] } deriving (Show)

instance InputParser Direction where
    parseInput "L" = DLeft
    parseInput "R" = DRight
    parseInput "U" = DUp
    parseInput "D" = DDown
    parseInput _   = undefined

instance InputParser Command where
    parseInput st = case words st of
                        [dir, steps] -> Command (parseInput dir) (read steps)
                        _            -> undefined
                        
instance InputParser Commands where
    parseInput = Commands . map parseInput . lines

type Pos = (Int,Int)
newtype Rope = Rope [Pos] deriving (Show)
type Movement = (Int,Int)

ropeInitialState :: Int -> Rope
ropeInitialState n = Rope (replicate n (0,0))

ropeTail :: Rope -> Pos
ropeTail (Rope xs) = last xs

asMovement :: Direction -> Movement
asMovement DLeft  = (-1, 0)
asMovement DRight = ( 1, 0)
asMovement DUp    = ( 0, 1)
asMovement DDown  = ( 0,-1)

applyMovement :: Movement -> Pos -> Pos
applyMovement (dx,dy) (x,y) = (x+dx,y+dy)

applyDirection :: Direction -> Pos -> Pos
applyDirection dir = applyMovement (asMovement dir)

moveHead :: Direction -> Rope -> Rope
moveHead dir (Rope (x:xs)) = Rope (applyDirection dir x : xs)
moveHead _ _ = undefined

movementTo :: Pos -> Pos -> Movement
movementTo (xt,yt) (xo,yo) = (xt-xo,yt-yo)

manhattanDistance :: Movement -> Int
manhattanDistance (dx,dy) = abs dx + abs dy

sign :: Integral a => a -> a
sign a
    | a < 0     = -1
    | a > 0     =  1
    | otherwise =  0

diagonalize :: Movement -> Movement
diagonalize (dx,dy) = (sign dx, sign dy)

correction :: Movement -> Maybe Movement
correction (-2, 0) = Just (-1, 0)
correction ( 2, 0) = Just ( 1, 0)
correction ( 0, 2) = Just ( 0, 1)
correction ( 0,-2) = Just ( 0,-1)
correction mov
    | manhattanDistance mov <= 2 = Nothing
    | otherwise                  = Just (diagonalize mov)

followHead :: Pos -> Pos -> Pos
followHead h t = 
    let movement = movementTo h t
        correctionMovement = correction movement
        newTail = case correctionMovement of
            Just mov -> applyMovement mov t
            Nothing  -> t
    in  newTail

adjustTail :: Rope -> Rope
adjustTail (Rope []) = Rope []
adjustTail (Rope [x]) = Rope [x]
adjustTail (Rope (x:y:ys)) = Rope (x:newTail)
    where newSegment   = followHead x y
          Rope newTail = adjustTail (Rope (newSegment:ys))

moveRope :: Direction -> Rope -> Rope
moveRope dir = adjustTail . moveHead dir

moveRopeTracked :: Direction -> Rope -> Writer [Pos] Rope
moveRopeTracked dir rope = do
    let newRope = moveRope dir rope
    tell [ropeTail newRope]
    return newRope

applyCommand :: Command -> Rope -> Writer [Pos] Rope
applyCommand (Command _ 0) rope = return rope
applyCommand (Command d s) rope = applyCommand (Command d (s-1)) rope >>= moveRopeTracked d

applyCommands :: Commands -> Rope -> Writer [Pos] Rope
applyCommands (Commands []    ) rope = return rope
applyCommands (Commands (c:cs)) rope = applyCommand c rope >>= applyCommands (Commands cs)

runCommands :: Int -> Commands -> Writer [Pos] Rope
runCommands n cmds = do
    let initialState = ropeInitialState n
    tell [ropeTail initialState]
    applyCommands cmds initialState

countTailPositions :: Writer [Pos] Rope -> Int
countTailPositions = length . nub . snd . runWriter

main :: IO ()
main = solvePuzzle 9 solver

solver :: Commands -> (Int, Int)
solver cmds = ( countTailPositions . runCommands 2  $ cmds
              , countTailPositions . runCommands 10 $ cmds
              )