module BinarySearch (find) where

import Data.Array

find' :: Ord a => Array Int a -> (Int, Int) -> a -> Maybe Int
find' a (i, j) e =
  if i > j
    then Nothing
    else let k = div (i + j) 2 in case compare e (a ! k) of
      LT -> find' a (i, pred k) e
      EQ -> return k
      GT -> find' a (succ k, j) e

find :: Ord a => Array Int a -> a -> Maybe Int
find = find' <*> bounds
