module Main where

import Data.List.Split
import qualified Data.Set as Set

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

splitOnHyphen :: String -> [Int]
splitOnHyphen x = map (\x -> read x :: Int) (splitOn "-" x)

toRange :: [Int] -> [Int]
toRange [x, y] = [x .. y]

-- shortest of the ranges equal intersect
minOfRanges :: Set.Set Int -> Set.Set Int -> Int
minOfRanges x y = min (length x) (length y)

toSet :: [Int] -> Set.Set Int
toSet x = Set.fromList x

toSections :: String -> Set.Set Int
toSections x = toSet $ toRange $ splitOnHyphen x

overlap :: Set.Set Int -> Set.Set Int -> Set.Set Int
overlap x y = Set.intersection x y

fullyContained :: Set.Set Int -> Set.Set Int -> Bool
fullyContained x y = (length (overlap x y)) == (minOfRanges x y)

sumTrue :: [Bool] -> Int
sumTrue xs = foldl (\acc x -> if x == True then acc + 1 else acc) 0 xs

hasOverlap :: Set.Set Int -> Set.Set Int -> Bool
hasOverlap x y = (length (overlap x y)) > 0

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day4/input"
  let sectionPairs =
        map (\[x, y] -> ((toSections x), (toSections y))) $
          map (\x -> splitOn "," x) content
  let pairsFullyContained =
        sumTrue $
          map (\(x, y) -> fullyContained x y) sectionPairs
  print pairsFullyContained
  let sectionsThatOverlap = sumTrue $ map (\(x, y) -> hasOverlap x y) sectionPairs
  print sectionsThatOverlap
