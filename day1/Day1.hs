module Main where

import Text.Read

readMaybeInt :: String -> Maybe Int
readMaybeInt x = readMaybe x

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

sumCaloriesPerElf :: Int -> [Maybe Int] -> [Int]
sumCaloriesPerElf _ [] = []
sumCaloriesPerElf x (Nothing : ys) = x : sumCaloriesPerElf 0 ys
sumCaloriesPerElf x (Just y : ys) = sumCaloriesPerElf (x + y) ys

topThreeByTotalCalories :: Int -> Int -> Int -> [Int] -> (Int, Int, Int)
topThreeByTotalCalories i j k [] = (i, j, k)
topThreeByTotalCalories i j k (x : xs)
  | i < x = topThreeByTotalCalories x i j xs
  | j < x = topThreeByTotalCalories i x j xs
  | k < x = topThreeByTotalCalories i j x xs
  | otherwise = topThreeByTotalCalories i j k xs

sumTopThree :: (Int, Int, Int) -> Int
sumTopThree (x, y, z) = x + y + z

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day1/input"
  let caloriesPerItem = map readMaybeInt content
  let totalCaloriesPerElf = sumCaloriesPerElf 0 caloriesPerItem
  let maxTotalCalories = maximum $ totalCaloriesPerElf
  let topThree = sumTopThree $ topThreeByTotalCalories 0 0 0 totalCaloriesPerElf
  print maxTotalCalories
  print topThree
