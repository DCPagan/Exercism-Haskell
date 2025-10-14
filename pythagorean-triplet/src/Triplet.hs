{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Triplet (tripletsWithSum) where

import Control.Monad

import Data.Maybe (mapMaybe)

import GHC.Real

{-|
  a^2 + b^2 = c^2
  a + b + c = n
  a + b + sqrt (a^2 + b^2) = n
  sqrt (a^2 + b^2) = n - a - b
  a^2 + b^2 = n^2 + a^2 + b^2 - n * (a + b) - a * (n - b) - b * (n - a)
  0 = n^2 - n * (a + b) - a * (n - b) - b * (n - a)
  n * (a + b) + a * (n - b) + b * (n - a) = n^2
  2 * n * a + 2 * n * b - 2 * a * b = n^2
  2 * n * a + 2 * b * (n - a) = n^2
  2 * b * (n - a) = n^2 - 2 * n * a
  b = (n^2 - 2 * n * a) / (2 * (n - a))
  b = (n * (n - 2 * a)) / (2 * (n - a))

  Ergo a^2 + b^2 = c^2 and a + b + c = n
    ==> b = (n * (n - 2 * a)) / (2 * (n - a))
-}
pythagorean :: (Integral a, Floating b) => a -> a -> b
pythagorean a b = sqrt $ fromIntegral a ^ 2 + fromIntegral b ^ 2

maybeIntegral :: (Real a, Integral b) => a -> Maybe b
maybeIntegral x = case toRational x of
  y :% 1 -> Just $ fromIntegral y
  _ -> Nothing

getOpposite :: (Integral a) => a -> a -> Maybe a
getOpposite n a =
  if n == a
    then Nothing
    else maybeIntegral $ toRational (n * (n - 2 * a)) / toRational
      (2 * (n - a))

getPair :: (Integral a) => a -> a -> Maybe (a, a)
getPair n = sequence . ((,) <*> getOpposite n)

triplet :: (Integral a) => a -> a -> Maybe (a, a, a)
triplet a b = (a, b, ) <$> maybeIntegral (pythagorean a b)

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n =
  filter (\(a, b, _) -> a <= b) . mapMaybe (uncurry triplet <=< getPair n) $
  enumFromTo 1 $ div n 2
