{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List.Split

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Shape = Rock | Paper | Scissor deriving (Eq, Show, Enum, Bounded)
data Outcome = Win | Loss | Draw deriving (Eq, Show, Enum, Bounded)

win :: Shape -> Shape
win x = case x of
  Rock -> Paper
  Paper -> Scissor
  Scissor -> Rock

outcomeScore :: Outcome -> Int
outcomeScore x = case x of
  Win -> 6
  Draw -> 3
  Loss -> 0

shapeScore :: Shape -> Int
shapeScore x = case x of
  Rock -> 1
  Paper -> 2
  Scissor -> 3

score :: Shape -> Shape -> Int
score opponent player = case (opponent, player) of
  (Rock, Paper) -> outcomeScore Win + shapeScore Paper
  (Rock, Scissor) -> outcomeScore Loss + shapeScore Scissor
  (Rock, Rock) -> outcomeScore Draw + shapeScore Rock
  (Paper, Paper) -> outcomeScore Draw + shapeScore Paper
  (Paper, Scissor) -> outcomeScore Win + shapeScore Scissor
  (Paper, Rock) -> outcomeScore Loss + shapeScore Rock
  (Scissor, Paper) -> outcomeScore Loss + shapeScore Paper
  (Scissor, Scissor) -> outcomeScore Draw + shapeScore Scissor
  (Scissor, Rock) -> outcomeScore Win + shapeScore Rock

toShape :: String -> Shape
toShape x
  | elem x ["A", "X"] = Rock
  | elem x ["B", "Y"] = Paper
  | elem x ["C", "Z"] = Scissor
  | otherwise = error "Non valid input"

toOutcome :: String -> Outcome
toOutcome x = case x of
  "X" -> Loss
  "Y" -> Draw
  "Z" -> Win

toPlayerShape :: Outcome -> Shape -> Shape
toPlayerShape o x = case (o, x) of
  (Loss, Rock) -> Scissor
  (Loss, Scissor) -> Paper
  (Loss, Paper) -> Rock
  (Draw, Rock) -> Rock
  (Draw, Scissor) -> Scissor
  (Draw, Paper) -> Paper
  (Win, Rock) -> Paper
  (Win, Scissor) -> Rock
  (Win, Paper) -> Scissor

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day2/input"
  let y =
        sum $
          map (\(x, y) -> score x y) $
            map (\[x, y] -> (toShape x, toShape y)) $
              map (\x -> splitOn " " x) content
  print y
  let z =
        sum $
          map (\(x, y) -> score x y) $
            map (\(x, y) -> (x, toPlayerShape y x)) $
              map (\[x, y] -> (toShape x, toOutcome y)) $
                map (\x -> splitOn " " x) content
  print z
