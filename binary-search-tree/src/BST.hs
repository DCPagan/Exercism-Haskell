{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

import Data.Bifunctor.TH
import Data.Fix
import Data.List hiding (insert, singleton)

data BSTF a b where
  Empty :: BSTF a b
  Node :: a -> b -> b -> BSTF a b
  deriving (Functor, Eq, Show)
deriveBifunctor ''BSTF

type BST a = Fix (BSTF a)

instance {-# OVERLAPPING #-} Show a => Show (BST a) where
  show (Fix Empty) = "Empty"
  show (Fix (Node a b c)) = "Node " ++ show a
    ++ "(" ++ show b ++ ")" ++ "(" ++ show c ++ ")"

instance {-# OVERLAPPING #-} Eq a => Eq (BST a) where
  Fix Empty == Fix Empty = True
  Fix (Node a b c) == Fix (Node d e f) = a == d && b == e && c == f
  _ == _ = False

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Fix (Node _ x _)) = Just x
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Fix (Node _ _ x)) = Just x
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (Fix (Node x _ _)) = Just x
bstValue _ = Nothing

empty :: BST a
empty = Fix Empty

singleton :: a -> BST a
singleton a = Fix $ Node a empty empty

insert :: Ord a => a -> BST a -> BST a
insert a (Fix Empty) = singleton a
insert a (Fix (Node b c d)) = Fix
  $ if (a <= b)
    then Node b (insert a c) d
    else Node b c (insert a d)

fromList :: Ord a => [a] -> BST a
fromList l
  | length l == 0 = empty
  | otherwise = Fix $ Node middle (fromList left) (fromList right)
    where
      sorted = sort l
      halfLen = div (length l) 2
      left = take halfLen sorted
      Just (middle, right) = uncons $ drop halfLen sorted

toList :: BST a -> [a]
toList (Fix Empty) = []
toList (Fix (Node a b c)) = toList b ++ [a] ++ toList c
