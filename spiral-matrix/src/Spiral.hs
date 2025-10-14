{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spiral (spiral) where

import Data.List

deosil :: [[a]] -> [[a]]
deosil = transpose . reverse

widdershins :: [[a]] -> [[a]]
widdershins = reverse . transpose

spiralBy :: (Integral a) => a -> a -> a -> [[a]]
spiralBy x y n =
  if
    | x == 0 -> []
    | y == 0 -> []
    | otherwise -> take (fromIntegral x) (enumFrom n):deosil
      (spiralBy (pred y) x (n + x))

spiral :: Int -> [[Int]]
spiral size = spiralBy size size 1
