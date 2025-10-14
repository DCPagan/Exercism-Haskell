{-# LANGUAGE MultiWayIf #-}
module RotationalCipher (rotate) where

import Data.Char

encode :: Int -> Char -> Char
encode k c = if
  | isUpper c -> chr $ (ord 'A' +) $ flip mod 26 $ (k +) $ ord c - ord 'A'
  | isLower c -> chr $ (ord 'a' +) $ flip mod 26 $ (k +) $ ord c - ord 'a'
  | otherwise -> c

rotate :: Int -> String -> String
rotate x = fmap (encode x)
