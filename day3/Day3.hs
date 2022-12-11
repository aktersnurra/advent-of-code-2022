module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

getCompartmentSize :: String -> Int
getCompartmentSize x = div (length x) 2

getCompartments :: String -> (String, String)
getCompartments x = splitAt (getCompartmentSize x) x

getDuplicate :: String -> String -> String
getDuplicate xs ys = [x | x <- xs, elem x ys]

toPriority :: HM.HashMap Char Int -> String -> [Maybe Int]
toPriority priority xs = [HM.lookup x priority | x <- xs]

groupElfs :: [String] -> [[String]]
groupElfs [] = []
groupElfs xs = (take 3 xs) : (groupElfs (drop 3 xs))

toSet :: String -> Set.Set Char
toSet x = Set.fromList x

findBadge :: String -> String -> String -> String
findBadge x y z = Set.toList $ (Set.intersection (Set.intersection (toSet x) (toSet y)) (toSet z))

sum' :: (Num a) => [Maybe a] -> a
sum' [] = 0
sum' (Just x : xs) = x + sum' xs
sum' (Nothing : xs) = sum' xs

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day3/input"
  let setNub xs = Set.toList $ Set.fromList xs
  let priority = HM.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 .. 52]
  let compartments = map (\(x, y) -> (setNub x, setNub y)) $ map getCompartments content
  let duplicates = map (\(xs, ys) -> getDuplicate xs ys) compartments
  let totalPriority = sum' $ concat $ map (\x -> toPriority priority x) duplicates
  print totalPriority
  let groupPriority =
        sum' $
          concat $
            map (\x -> toPriority priority x) $
              map (\[x, y, z] -> findBadge x y z) $
                groupElfs content
  print groupPriority
