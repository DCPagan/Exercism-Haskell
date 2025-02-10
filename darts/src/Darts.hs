{-# LANGUAGE MultiWayIf #-}
module Darts (score) where

innerRadius :: Floating a => a
innerRadius = 1

middleRadius :: Floating a => a
middleRadius = 5

outerRadius :: Floating a => a
outerRadius = 10

pythagorean :: Floating a => a -> a -> a
pythagorean a b = sqrt $ a ^ 2 + b ^ 2

score :: Float -> Float -> Int
score x y = if
  | radius <= innerRadius -> 10
  | radius <= middleRadius -> 5
  | radius <= outerRadius -> 1
  | otherwise -> 0
  where
    radius = pythagorean x y
