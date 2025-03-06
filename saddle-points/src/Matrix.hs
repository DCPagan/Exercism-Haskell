{-# LANGUAGE TupleSections #-}
module Matrix (saddlePoints) where

import Control.Arrow
import Control.Lens
import Data.Array
import Data.Semigroup

rowBounds :: Array (a, a) e -> (a, a)
rowBounds = each %~ fst <<< bounds

colBounds :: Array (a, a) e -> (a, a)
colBounds = each %~ snd <<< bounds

row :: Ix i => i -> Array (i, i) e -> Array i e
row i a = ixmap (colBounds a) (i,) a

col :: Ix i => i -> Array (i, i) e -> Array i e
col i a = ixmap (rowBounds a) (,i) a

maxOfRow :: (Ix i, Ord e) => i -> Array (i, i) e -> Maybe e
maxOfRow i = fmap getMax . foldMap (Just . Max) . row i

minOfCol :: (Ix i, Ord e) => i -> Array (i, i) e -> Maybe e
minOfCol i = fmap getMin . foldMap (Just . Min) . col i

isSaddlePoint :: (Ix i, Ord e) => Array (i, i) e -> (i, i) -> e -> Bool
isSaddlePoint a (i, j) e
  = Just e == maxOfRow i a && Just e == minOfCol j a

saddlePoints :: (Ix i, Ord e) => Array (i, i) e -> [(i, i)]
saddlePoints a
  = a ^.. itraversed . ifiltered (isSaddlePoint a) . withIndex . _1
