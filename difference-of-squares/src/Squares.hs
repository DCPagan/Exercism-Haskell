module Squares (difference, squareOfSum, sumOfSquares) where

import Control.Applicative (Applicative(..))

square :: Integral a => a -> a
square n = n * n

difference :: Integral a => a -> a
difference = liftA2 (-) squareOfSum sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum = square . sum . enumFromTo 1

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . fmap square . enumFromTo 1
