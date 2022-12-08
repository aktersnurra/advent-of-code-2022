{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Read
import Data.List.Split
-- optimal strategy
-- A -> Y
-- B -> Z
-- C -> X

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Hand = Rock | Paper | Scissor deriving (Eq, Show, Enum, Bounded)
data Outcome = Win | Loss | Draw deriving (Eq, Show, Enum, Bounded)

-- splitOn :: String -> [T.Text]
-- splitOn x = splitOn " " x :: T.Text

win :: Hand -> Hand
win x = case x of Rock -> Paper
                  Paper -> Scissor
                  Scissor -> Rock

outcomeScore :: Outcome -> Int
outcomeScore x = case x of Win -> 6
                           Draw -> 3
                           Loss -> 0

handScore :: Hand -> Int
handScore x = case x of Rock -> 1
                        Paper -> 2
                        Scissor -> 3

score :: Hand -> Hand -> Int
score opponent player = case (opponent,player) of (Rock,Paper) -> outcomeScore Win + handScore Paper
                                                  (Rock,Scissor) -> outcomeScore Loss + handScore Scissor
                                                  (Rock,Rock) -> outcomeScore Draw + handScore Rock
                                                  (Paper,Paper) -> outcomeScore Draw + handScore Paper
                                                  (Paper,Scissor) -> outcomeScore Win + handScore Scissor
                                                  (Paper,Rock) -> outcomeScore Loss + handScore Rock
                                                  (Scissor,Paper) -> outcomeScore Loss + handScore Paper
                                                  (Scissor,Scissor) -> outcomeScore Draw + handScore Scissor
                                                  (Scissor,Rock) -> outcomeScore Win + handScore Rock

toHand :: String -> Hand
toHand x = case x of x | elem x ["A","X"] -> Rock
                       | elem x ["B","Y"] -> Paper
                       | elem x ["C","Z"] -> Scissor
                       | otherwise -> error "Non valid input"

toOutcome :: String -> Outcome
toOutcome x = case x of "X" -> Loss
                        "Y" -> Draw
                        "Z" -> Win

toPlayerHand :: Outcome -> Hand -> Hand
toPlayerHand o x = case (o,x) of (Loss,Rock) -> Scissor
                                 (Loss,Scissor) -> Paper
                                 (Loss,Paper) -> Rock
                                 (Draw,Rock) -> Rock
                                 (Draw,Scissor) -> Scissor
                                 (Draw,Paper) -> Paper
                                 (Win,Rock) -> Paper
                                 (Win,Scissor) -> Rock
                                 (Win,Paper) -> Scissor

main :: IO ()
main = do
  content <- readLines "/home/aktersnurra/projects/advent-of-code-2022/day2/input"
  let y = sum $ map (\(x,y) -> score x y) $ map (\[x,y] -> (toHand x, toHand y)) $ map (\x -> splitOn " " x) content
  print y
  let z = sum $ map (\(x,y) -> score x y) $ map (\(x,y) -> (x, toPlayerHand y x)) $ map (\[x,y] -> (toHand x, toOutcome y)) $ map (\x -> splitOn " " x) content
  print z
