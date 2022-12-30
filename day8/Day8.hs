module Main where

import Data.List

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

toInts :: String -> [Int]
toInts xs = map (read . (:"")) xs

toGrid :: [String] -> [[Int]]
toGrid xs = map toInts xs

visable :: (Ord a) => a -> [a] -> Bool
visable _ [] = True
visable y xs = foldl (\acc x -> if x > y then True else acc) False xs

fn :: Int -> [Int] -> Bool
fn i row = visable x before || visable x after
  where
  x = row !! i
  -- fix if i = 0
  before = take (i-1) row
  after = drop (i+1) row

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day8/input"
  let grid = toGrid content
  let transposedGrid = transpose grid
  let i = head grid 
  -- let x = fn 3 i
  print i
  let x = take 0 i
  print x
  -- row
  -- colum
  -- print grid
  -- print transposedGrid
  -- let xs = head grid
  -- let y = xs !! 10
  -- let right = take 9 xs
  -- let left = drop 11 xs
  -- let a = visable y right
  -- let b = visable y left
  -- print a
  -- print b
