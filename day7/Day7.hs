module Main where

import Data.List

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

calculateDirSize :: Content -> Int
calculateDirSize (File size _) = size
calculateDirSize (Dir _ files) = sum . map calculateDirSize $ files

calculateDirSizes :: Content -> [Int]
calculateDirSizes (File _ _) = []
calculateDirSizes (Dir dir files) = calculateDirSize (Dir dir files) : foldl (\x y -> x ++ calculateDirSizes y) [] files

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day7/input"
  let dir = last $ words $ head content
  let root = Dir{dir = dir, files = []}
  let cmds = drop 1 content
  let (_, fs) = createFileSystem cmds root
  let dirSizes = calculateDirSizes fs
  let answer1 = sum . filter (< 100000) $ dirSizes
  print answer1
  let spaceNeeded = 30000000 - (70000000 - maximum dirSizes)
  let answer2 = minimum . filter (> spaceNeeded) $ dirSizes
  print answer2
