{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Palindromes (largestPalindrome, smallestPalindrome) where

import Control.Arrow ((<<<))
import Control.Monad (guard)

factorizeInRange :: Integral a => (a, a) -> a -> [(a, a)]
factorizeInRange (low, high) n
  | low > high = []
  | otherwise = do
    x <- enumFromTo low $ floor $ sqrt @Double $ fromIntegral n
    guard $ mod n x == 0
    let y = div n x
    guard $ y <= high
    return (x, y)

decimalDecomposition :: Integral a => a -> [a]
decimalDecomposition = fmap (`rem` 10) . takeWhile (0 /=) . iterate (`quot` 10)

isPalindrome :: Integral a => a -> Bool
isPalindrome = (==) <*> reverse <<< decimalDecomposition

largestPalindrome
  :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome low' high' = largestPalindrome' low' high' (high' * high')
  where
    largestPalindrome' low high pp
      | low * low > pp = Nothing
      | not $ isPalindrome pp =
        largestPalindrome' low high $ pred pp
      | otherwise = do
        let factors = factorizeInRange (low, high) pp
        if null factors
          then largestPalindrome' low high $ pred pp
          else return (pp, factors)

smallestPalindrome
  :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome low' high' = smallestPalindrome' low' high' (low' * low')
  where
    smallestPalindrome' low high _product
      | high * high < _product = Nothing
      | not $ isPalindrome _product =
        smallestPalindrome' low high $ succ _product
      | otherwise = do
        let _factors = factorizeInRange (low, high) _product
        if null _factors
          then smallestPalindrome' low high $ succ _product
          else return (_product, _factors)
