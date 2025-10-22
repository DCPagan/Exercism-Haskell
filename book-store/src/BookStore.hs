{-# LANGUAGE BlockArguments #-}

module BookStore (total, Book(..)) where

import Control.Arrow (Arrow(..))

import Data.Function (on)
import Data.IntMap ((!?), elems, fromListWith, insert, insertWith)
import Data.List (group, sort, sortBy, transpose)
import Data.Maybe (fromMaybe)

data Book
  = First
  | Second
  | Third
  | Fourth
  | Fifth
  deriving (Eq, Ord, Show, Enum, Bounded)

{-|
  Calculate the price of the size of a bundle.
-}
price :: [Book] -> Int
price bundle = case length bundle of
  1 -> 800
  2 -> 1520
  3 -> 2160
  4 -> 2560
  5 -> 3000
  x -> 800 * x

{-|
  Divide the basket to the largest bundles in descending order of length.
-}
largestBundles :: [Book] -> [[Book]]
largestBundles =
  fmap sort . transpose . sortBy (on (flip compare) length) . group . sort

{-|
  Difference between two sorted lists split from the first list.
-}
differenceSplit :: Ord a => [a] -> [a] -> ([a], [a])
differenceSplit [] _ = ([], [])
differenceSplit a [] = (a, [])
differenceSplit (x : xs) (y : ys) = case compare x y of
  LT -> first (x :) $ differenceSplit xs (y : ys)
  EQ -> second (x :) $ differenceSplit xs ys
  GT -> first (y :) $ differenceSplit (x : xs) ys

toFours :: Ord a => [a] -> [a] -> [[a]]
toFours five three = case differenceSplit five three of
  ([a, b], three') -> [a : three, b : three']
  _ -> [five, three]

{-|
  Redistribute books from bundles of 5 to bundles of 3.
-}
redistributeBundles :: [[Book]] -> [[Book]]
redistributeBundles l = fromMaybe l do
  let bundlesMap = fromListWith (++) $ (length &&& return) <$> l
  fives' <- bundlesMap !? 5
  threes' <- bundlesMap !? 3
  let len = on min length fives' threes'
      (a, fives) = splitAt len fives'
      (b, threes) = splitAt len threes'
      fours = concat $ zipWith toFours a b
      bundlesMap' =
        insert 5 fives $ insert 3 threes $ insertWith (++) 4 fours bundlesMap
      bundles = concat $ elems bundlesMap'
  return bundles

total :: [Book] -> Int
total = sum . fmap price . redistributeBundles . largestBundles
