module Frequency (frequency) where

import Control.Arrow (Arrow(..))
import Control.Parallel.Strategies (parList, rpar, rseq, runEval)

import Data.Char (isAlpha, toLower)
import Data.Map.Strict (Map, empty, insertWith, unionsWith)
import Data.Text (Text, foldl')

countChar :: Map Char Int -> Char -> Map Char Int
countChar m c =
  if isAlpha c
    then insertWith (+) (toLower c) 1 m
    else m

countChars :: Text -> Map Char Int
countChars = foldl' countChar empty

pred' :: Int -> Int
pred' r
  | r <= 0 = r
  | otherwise = pred r

succ' :: Int -> Int -> Int
succ' n r
  | r <= 0 = n
  | otherwise = succ n

chunk' :: Int -> Int -> [a] -> [[a]]
chunk' n r l
  | null l = []
  | otherwise =
    uncurry (:) $ second (chunk' n (pred' r)) $ splitAt (succ' n r) l

chunks :: Int -> [a] -> [[a]]
chunks n l
  | n <= 1 = [l]
  | otherwise = chunk' len remnant l
  where
    len = div (length l) n

    remnant = mod (length l) n

freq :: [Text] -> Map Char Int
freq = unionsWith (+) . fmap countChars

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = runEval $ do
  x <- parList rpar $ freq <$> chunks nWorkers texts
  rseq $ unionsWith (+) x
