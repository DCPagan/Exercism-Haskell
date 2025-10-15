{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Alphametics (solve) where

import Data.Char
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read

{-|
  | SEND
  | MORE
  |MONEY

  nub $ filter isLetter "sendmoremoney" ==> "sendmory"

    S E N D M O R Y 1
  S 1       1 -     0
  E   1 -     1     0
  N   - 1       1   0
  D   1   1       - 0
  M         1       -
  O                 0
  R                 0
  Y                 0
-}
data Alphametics where
  Alphametics :: { _addenda :: [String], _total :: String } -> Alphametics

plus :: ReadP ()
plus = do
  skipSpaces
  char '+'
  skipSpaces

alphametics :: ReadP Alphametics
alphametics = do
  _addenda <- sepBy (fmap toUpper <$> munch isAlpha) plus
  skipSpaces
  string "=="
  skipSpaces
  _total <- fmap toUpper <$> munch isAlpha
  return Alphametics { .. }

instance Read Alphametics where
  readPrec = lift alphametics

type AlphaMap = M.Map Char [Int]

initAlpha :: String -> AlphaMap
initAlpha = M.fromList . fmap (, [0..9]) . nub . filter isAlpha

deosil :: [[a]] -> [[a]]
deosil = transpose . reverse

widdershins :: [[a]] -> [[a]]
widdershins = reverse . transpose

padRows :: [[a]] -> [[Maybe a]]
padRows [] = []
padRows l =
  let
    m = maximum $ length <$> l
    f r = replicate (m - length r) Nothing ++ fmap Just r
  in
    fmap f l

padColumns :: (Ord a) => [[a]] -> [[Maybe a]]
padColumns = fmap (sortBy $ flip compare) . transpose . padRows

maxPossibleColumnValue :: (Ord a) => [Maybe a] -> Int
maxPossibleColumnValue =
  sum . zipWith (*) [9, 8..0] . fmap length . group . catMaybes

maxCarry :: (Ord a) => [Maybe a] -> Int
maxCarry = flip div 10 . maxPossibleColumnValue

solve' :: AlphaMap -> Alphametics -> Maybe [(Char, Int)]
solve' _m Alphametics { .. } =
  if length _total <= length (transpose _addenda)
    then Nothing
    else undefined

solve :: String -> Maybe [(Char, Int)]
solve puzzle = readMaybe puzzle >>= solve' (initAlpha puzzle)
