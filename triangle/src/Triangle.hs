{-# LANGUAGE MultiWayIf #-}

module Triangle (TriangleType(..), triangleType) where

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangle' :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangle' a b c
  | a + b <= c = Illegal
  | a == b =
    if b == c
      then Equilateral
      else Isosceles
  | b == c = Isosceles
  | otherwise = Scalene

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
  | a <= b =
    if
      | b <= c -> triangle' a b c
      | a <= c -> triangle' a c b
      | otherwise -> triangle' c a b
  | c <= a =
    if b <= c
      then triangle' b c a
      else triangle' c b a
  | otherwise = triangle' b a c
