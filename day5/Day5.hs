module Main where

import Data.List.Split
import qualified Data.Map as Map
import Text.Read (readMaybe)

readMaybeInt :: String -> Maybe Int
readMaybeInt x = readMaybe x

data Rearrange = Rearrange {move :: Int, from :: Int, to :: Int} deriving (Show)

toRearrange :: [Int] -> Rearrange
toRearrange [x, y, z] = Rearrange x y z

parseRearrangement :: [String] -> Rearrange
parseRearrangement xs = toRearrange $ [x | Just x <- [readMaybeInt x | x <- xs]]

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseLayer :: String -> [String]
parseLayer [] = []
parseLayer xs = (filter (`elem` ['A' .. 'Z']) $ (take 3 xs)) : (parseLayer (drop 4 xs))

insert :: Int -> [v] -> Map.Map Int [v] -> Map.Map Int [v]
insert i stack stacks = Map.insert i stack stacks

get :: Int -> Map.Map Int [v] -> [v]
get i stacks = stacks Map.! i

pop :: Rearrange -> Map.Map Int [v] -> ([v], Map.Map Int [v])
pop x y = (pop, updatedStack)
 where
  stack = get (from x) y
  pop = drop ((length stack) - (move x)) stack
  keep = take ((length stack) - (move x)) stack
  updatedStack = insert (from x) keep y

push :: Rearrange -> [v] -> Map.Map Int [v] -> Map.Map Int [v]
push x y z = updatedStacks
 where
  stack = get (to x) z
  updated = (++) stack y
  updatedStacks = insert (to x) updated z

moveCrates :: Rearrange -> Map.Map Int [v] -> Map.Map Int [v]
moveCrates x y = updated
 where
  (j, k) = pop x y
  updated = push x (reverse j) k

moveCrates9001 :: Rearrange -> Map.Map Int [v] -> Map.Map Int [v]
moveCrates9001 x y = updated
 where
  (j, k) = pop x y
  updated = push x j k

topCrates :: Map.Map Int [v] -> [v]
topCrates x = y
 where
  y = map (\k -> (last $ get k x)) [1 .. 9]

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day5/input"
  let rowsOfCrates = filter (\(_, y) -> y /= "") $ concat $ map (\x -> zip [1 ..] x) $ map parseLayer $ take 8 content
  let stacks = Map.fromListWith (++) [(k, [v]) | (k, v) <- rowsOfCrates]
  let rearrangements = map parseRearrangement $ map (splitOn " ") $ drop 10 content
  let finalStack = foldl (\acc x -> moveCrates x acc) stacks rearrangements
  print finalStack
  let y = concat $ topCrates finalStack
  print y

  let finalStack9001 = foldl (\acc x -> moveCrates9001 x acc) stacks rearrangements
  print finalStack9001
  let z = concat $ topCrates finalStack9001
  print z
