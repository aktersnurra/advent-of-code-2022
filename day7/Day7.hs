module Main where

import Data.List.Split
import Text.Read (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt x = readMaybe x

-- parse :: String ->

data File = File {size :: Int, name :: String} deriving (Show)

data Dir = Dir {parent :: Maybe Dir, dir :: String, files :: [File], dirs :: [Dir]} deriving (Show)

splitOnSpace = splitOn " "

addFiles :: Dir -> [File] -> Dir
addFile x y = x{files = ((files x) ++ y)}

addDirs :: Dir -> [Dir] -> Dir
addDir x y = x{dirs = ((dirs x) ++ y)}

createDir :: Maybe Dir -> String -> Dir
createDir x y = Dir{parent = x, dir = y, files = [], dirs = []}

createDirs :: Maybe Dir -> [String] -> [Dir]
createDirs parent xs =
  map (\x -> createDir parent x) $
    map (\x -> last $ splitOnSpace x) $
      filter (\x -> (take 3 x) == "dir") $
        takeWhile (\x -> (head x) /= '$') xs

createFile :: String -> String -> File
createFile x y = File{size = read x :: Int, name = y}

createFiles :: [String] -> [File]
createFiles xs =
  map (\[x, y] -> createFile x y) $
    map (\x -> splitOnSpace x) $
      filter (\x -> (take 3 x) /= "dir") xs

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

-- parseFiles

-- parseDirs :: [String] -> [Dir]
-- parseDirs

-- tree :: Dir -> [String] -> ([Dir], [String])
-- tree (x:xs)
--   | isPrefixOf ""

-- parse :: [String] -> [Dir]
-- parse (x:xs)
--   | isPrefixOf x "$ cd" =
--   | isPrefixOf x "$ ls"  = takeWhile changeDir xs
--   | otherwise = []

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day7/input"
  let f = splitOn " "
  let x = take 13 content
  let y = drop 2 x
  -- let z = map (\x -> last $ f x) $ filter (\x -> isPrefixOf x "dir") $ takeWhile (\x -> (head x) /= '$') y
  let root = Just Dir{parent = Nothing, dir = "/", files = [], dirs = []}
  let z = createDirs root y
  let xx = take 4 $ drop 15 content
  let yy = createFiles xx
  print yy
  print xx
