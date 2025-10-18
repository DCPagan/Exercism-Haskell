{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sieve (primesUpTo) where

import Control.Arrow ((<<<), Arrow(..))

import Data.List (uncons)
import Data.Maybe (catMaybes)

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding ((/), div, divMod, gcd, lcm, mod, quot, quotRem, rem)

{-|
  Recursively call splitAt, failing the tail's head.
-}
splitOut :: Int -> [Maybe a] -> [Maybe a]
splitOut n
  | n <= 0 = fmap (const Nothing)
  | otherwise =
    uncurry (++) . second
      (maybe [] (uncurry (:) <<< const Nothing *** splitOut n) . uncons)
    . splitAt n

primesUpTo :: Integer -> [Integer]
primesUpTo = catMaybes . foldr sieve [] . fmap Just . enumFromTo 2
  where
    sieve Nothing t = t
    sieve (Just h) t = Just h:splitOut (fromIntegral $ pred h) t
