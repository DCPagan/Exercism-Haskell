{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Knapsack (maximumValue) where

import Control.Lens
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.List
import qualified Data.Set as S

data Item where
  Item ::
    { _weight :: Int
    , _value :: Int
    , _serial :: Int
    } -> Item
  deriving (Eq, Ord, Show)
makeLenses ''Item

type Knapsack = S.Set Item

toKnapsack :: [(Int, Int)] -> Knapsack
toKnapsack =
  S.fromList . zipWith (\_serial (_weight, _value) -> Item { .. }) (enumFrom 0)

maximumWeight :: Int -> Knapsack -> Int
maximumWeight n = min n . sumOf (folded . weight)

maximumValue :: Int -> [(Int, Int)] -> Int
maximumValue capacity items
  | sumOf (folded . _1) items <= capacity = sumOf (folded . _2) items
  | otherwise = sumOf (ix n . folded . value)
    $ runSTArray $ knapsack n k
    where
      k = toKnapsack items
      n = maximumWeight capacity k

knapsack :: MArray a Knapsack m
  => Int -> Knapsack -> m (a Int Knapsack)
knapsack capacity items = do
  z <- newArray (0, capacity) S.empty
  foldrM f z $ sort $ S.toList items
  where
    f :: MArray a Knapsack m => Item -> a Int Knapsack -> m (a Int Knapsack)
    f item@Item { .. } a =
      a <$ (getAssocs a >>= mapM_ (\(i, e) -> g i e >>= writeArray a i))
      where
        g cap withoutItem =
          if cap < _weight
          then return withoutItem
          else do
            withItem <- S.insert item <$> readArray a (cap - _weight)
            return $
              if sumOf (folded . value) withItem
                > sumOf (folded . value) withoutItem
              then withItem
              else withoutItem
