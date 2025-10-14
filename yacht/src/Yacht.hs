{-# LANGUAGE LambdaCase #-}

module Yacht (yacht, Category(..)) where

import Data.List

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

ones :: [Int] -> Int
ones = sum . filter (1 ==)

twos :: [Int] -> Int
twos = sum . filter (2 ==)

threes :: [Int] -> Int
threes = sum . filter (3 ==)

fours :: [Int] -> Int
fours = sum . filter (4 ==)

fives :: [Int] -> Int
fives = sum . filter (5 ==)

sixes :: [Int] -> Int
sixes = sum . filter (6 ==)

fullHouse :: [Int] -> Int
fullHouse l = case group $ sort l of
  [x, _]
    | length x == 2 -> sum l
  [x, _]
    | length x == 3 -> sum l
  _ -> 0

fourOfAKind :: [Int] -> Int
fourOfAKind l = case group $ sort l of
  x:_
    | length x >= 4 -> sum $ take 4 x
  [_, y]
    | length y == 4 -> sum y
  _ -> 0

littleStraight :: [Int] -> Int
littleStraight l = case sort l of
  [1, 2, 3, 4, 5] -> 30
  _ -> 0

bigStraight :: [Int] -> Int
bigStraight l = case sort l of
  [2, 3, 4, 5, 6] -> 30
  _ -> 0

choice :: [Int] -> Int
choice = sum

yacht' :: [Int] -> Int
yacht' l =
  if length (group l) == 1
    then 50
    else 0

yacht :: Category -> [Int] -> Int
yacht = \case
  Ones -> ones
  Twos -> twos
  Threes -> threes
  Fours -> fours
  Fives -> fives
  Sixes -> sixes
  FullHouse -> fullHouse
  FourOfAKind -> fourOfAKind
  LittleStraight -> littleStraight
  BigStraight -> bigStraight
  Choice -> choice
  Yacht -> yacht'
