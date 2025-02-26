module SumOfMultiples (sumOfMultiples) where

import Data.List

findMultiples :: Integral a => a -> a -> [a]
findMultiples _ x | x <= 0 = [0]
findMultiples limit base = takeWhile (< limit) $ iterate (base +) base

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ foldr union [] $ fmap (findMultiples limit) $ factors
