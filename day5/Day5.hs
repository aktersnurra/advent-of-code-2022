module Main where

import Data.List.Split
import qualified Data.Map as Map
import Text.Read

readMaybeInt :: String -> Maybe Int
readMaybeInt x = readMaybe x

data Rearrange = Rearrange {numCrates :: Int, from :: Int, to :: Int} deriving (Show)

toRearrange :: [Int] -> Rearrange
toRearrange [x,y,z] = Rearrange x y z

parseRearrangement :: [String] -> Rearrange
parseRearrangement xs = toRearrange $ [x | Just x <-[readMaybeInt x | x <- xs]]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseLayer :: String -> [String]
parseLayer [] = []
parseLayer xs = (take 3 xs):(parseLayer (drop 4 xs))

addCrate :: [v] -> [v] -> [v]
addCrate newCrate crates = crates ++ newCrate

removeCrate :: [v] -> [v]
removeCrate xs = drop 1 xs

main :: IO ()
main = do
  -- content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day5/input"
  -- let rowsOfCrates = filter (\(_,y) -> y /= "   ") $ concat $ map (\x -> zip [1..] x) $ map parseLayer $ take 8 content
  -- print rowsOfCrates
  -- let crates = Map.fromListWith (++) [ (k, [v]) | (k, v) <- rowsOfCrates]
  -- print crates
  -- let cmds = map parseRearrangement $ map (splitOn " ") $ drop 10 content
  -- print cmds
  let x = Map.singleton 1 ["a", "b"]
  -- let z = Map.insertWith addCrate 1 ["c"] z
  print x
  let y = Map.adjust removeCrate 1 x
  print y
