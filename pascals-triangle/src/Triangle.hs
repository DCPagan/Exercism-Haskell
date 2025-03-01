{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Triangle where

import Data.Maybe

headOrZero :: Num a => [a] -> a
headOrZero = fromMaybe 0 . listToMaybe

pascal :: (Integral a, Integral b) => a -> [b]
pascal n
  | n <= 0 = []
  | otherwise = 1 : (snd $ foldr f ([], []) $ pascal $ pred n)
    where
      f :: Num a => a -> ([a], [a]) -> ([a], [a])
      f a (b, c) = (a : b, a + headOrZero b : c)

rows :: Int -> [[Integer]]
rows = fmap pascal . enumFromTo 1
