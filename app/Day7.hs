module Main where

import Data.Char (isSpace)
import Data.List (groupBy)

import Utils

type Name = String
type Size = Int

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
data FSZipper = FSZipper FSItem [FSCrumb] deriving (Show)

emptyZipper :: FSZipper
emptyZipper = FSZipper (Folder "/" []) []

noZipper :: FSZipper -> FSItem
noZipper (FSZipper x _) = x

fsUp :: FSZipper -> FSZipper
fsUp (FSZipper item (FSCrumb name ls rs : bs)) = FSZipper (Folder name (ls ++ [item] ++ rs)) bs
fsUp _ = undefined

fsRoot :: FSZipper -> FSZipper
fsRoot z@(FSZipper _ []) = z
fsRoot z@(FSZipper (Folder _ _) _) = fsRoot . fsUp $ z
fsRoot _ = undefined

fsDown :: Name -> FSZipper -> FSZipper
fsDown name (FSZipper (Folder folderName items) bs) =
    case break (nameIs name) items of
        (ls, item:rs) -> FSZipper item (FSCrumb folderName ls rs : bs)
        _             -> undefined
fsDown _ _ = undefined

nameIs :: Name -> FSItem -> Bool
nameIs name (File fileName _)     = name == fileName
nameIs name (Folder folderName _) = name == folderName

fsCreateItem :: FSItem -> FSZipper -> FSZipper
fsCreateItem i (FSZipper (Folder folderName is) bs) = FSZipper (Folder folderName (i : is)) bs
fsCreateItem _ _ = undefined

fsSize :: FSItem -> Size
fsSize (File _ size) = size
fsSize (Folder _ cs) = sum . map fsSize $ cs

folders :: FSItem -> [FSItem]
folders (File _ _) = []
folders f@(Folder _ cs) = f : (concat . map folders $ cs)

data CdTarget = Subdir Name | Parent | Root deriving (Show)
data Command = Cd CdTarget | Ls [FSItem] deriving (Show)
newtype Commands = Commands { getCommands :: [Command] } deriving (Show)

parseCd :: String -> Command
parseCd args
    | args == "/"  = Cd Root
    | args == ".." = Cd Parent
    | otherwise    = Cd (Subdir args)

parseLs :: String -> Command
parseLs = Ls . map toFSItem . tail . lines
    where toFSItem st = case words st of
                            ["dir", folderName] -> Folder folderName []
                            [size, fileName]    -> File fileName (read size)
                            _                   -> undefined

instance InputParser Command where
    parseInput st = case break isSpace st of
                        ("cd", _:arg)  -> parseCd arg
                        ("ls", output) -> parseLs output
                        _              -> undefined

instance InputParser Commands where
    parseInput = Commands . map parseInput . map strip . groupBy (\_ x -> x /= '$')
        where strip x = let l = length x
                        in  drop 2 . take (l-1) $ x

main :: IO ()
main = solvePuzzle 7 solver

solver :: Commands -> (Size, Size)
solver cs = let fs = applyCommands cs
                capacity = 70000000
                availableSpace = capacity - fsSize fs
                requiredSpace = 30000000
                minimumDeleted = requiredSpace - availableSpace
            in  ( sum . filter (<= 100000) . map fsSize . folders $ fs
                , minimum . filter (>= minimumDeleted) . map fsSize . folders $ fs
                )

applyCommand :: Command -> FSZipper -> FSZipper
applyCommand (Cd Root)          z = fsRoot                               z
applyCommand (Cd Parent)        z = fsUp                                 z
applyCommand (Cd (Subdir name)) z = fsDown name                          z
applyCommand (Ls ([]))          z =                                      z
applyCommand (Ls (c:cs))        z = fsCreateItem c (applyCommand (Ls cs) z)

applyCommands :: Commands -> FSItem
applyCommands = noZipper . fsRoot . foldl (\z c -> applyCommand c z) emptyZipper . getCommands