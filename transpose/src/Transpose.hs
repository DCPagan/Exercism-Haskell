module Transpose (transpose) where

import Control.Arrow (Arrow(..))

import Data.List (uncons)
import Data.Maybe (mapMaybe)
import Data.Monoid ()

pad :: [String] -> [String]
pad = snd . foldr f (0, [])
  where
    f s (p, t) = (max p len, (s ++ replicate (p - len) ' '):t)
      where
        len = length s

transpose :: [String] -> [String]
transpose = f . mapMaybe uncons . pad
  where
    f [] = []
    f s = uncurry (:) $ second transpose $ unzip s
