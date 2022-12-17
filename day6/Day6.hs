module Main where

import qualified Data.Set as Set

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

findSOS :: Int -> Int -> String -> String
findSOS x y z
  | l == x = j
  | otherwise = findSOS x (y + 1) z
  where
  j = take y z
  k = Set.toList $ Set.fromList $ drop (length j - x) j
  l = length k

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day6/input"
  let msg = head content
  let sos = findSOS 4 4 msg
  print sos
  print $ length sos
  let som = findSOS 14 14 msg
  print som
  print $ length som
