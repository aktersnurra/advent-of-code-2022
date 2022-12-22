module Main where

import Data.List
import Data.List.Split
import qualified Data.Map as Map

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Content = File {size :: Int, fname :: String} | Dir {dir :: String, files :: [Content]} deriving (Show)

createFileSystem :: [String] -> Content -> ([String], Content)
createFileSystem [] fs = ([], fs)
createFileSystem (x : xs) (Dir dir files)
  | x == "$ cd .." = (xs, (Dir dir files))
  | x == "$ ls" = createFileSystem xs (Dir dir files)
  | isPrefixOf "$ cd" x = createFileSystem xss (Dir dir (sfs : files))
  | isPrefixOf "dir" x = createFileSystem xs (Dir dir files)
  | otherwise = createFileSystem xs (Dir dir (file : files))
 where
  [size, fname] = words x
  file = File (read size) fname
  dname = last $ words x
  (xss, sfs) = createFileSystem xs (Dir dname [])

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day7/input"
  let dir = last $ words $ head content
  let root = Dir{dir=dir, files=[]}
  let cmds = drop 1 content
  print cmds
  let (yy,xx) = createFileSystem cmds root
  print xx 
