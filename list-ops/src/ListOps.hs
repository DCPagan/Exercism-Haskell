{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat) where

import Control.Arrow (Arrow((&&&)))

import Data.Bifunctor
import Data.Maybe
import Data.Monoid

import Prelude hiding ((++), concat, filter, foldr, length, map, reverse)

data ListF a b where
  Nil :: ListF a b
  Cons :: a -> b -> ListF a b
  deriving (Eq, Show, Functor)

instance Bifunctor ListF where
  bimap f g = \case
    Nil -> Nil
    Cons a b -> Cons (f a) (g b)

project :: [a] -> ListF a [a]
project = \case
  [] -> Nil
  h:t -> Cons h t

embed :: ListF a [a] -> [a]
embed = \case
  Nil -> []
  Cons h t -> h:t

cata :: (ListF a b -> b) -> [a] -> b
cata alg = go
  where
    go = alg . fmap go . project

ana :: (a -> ListF b a) -> a -> [b]
ana coalg = go
  where
    go = embed . fmap go . coalg

para :: (ListF a ([a], b) -> b) -> [a] -> b
para alg = go
  where
    go = alg . fmap (id &&& go) . project

listAlgebra :: (a -> b -> b) -> b -> ListF a b -> b
listAlgebra f z = \case
  Nil -> z
  Cons a b -> f a b

listDualEndoAlgebra
  :: (a -> b -> b) -> ListF a (Dual (Endo b)) -> Dual (Endo b)
listDualEndoAlgebra f = listAlgebra ((<>) . Dual . Endo . f) (Dual $ Endo id)

listCoalgebra :: (a -> Maybe (b, [a])) -> (a -> ListF b [a])
listCoalgebra f = maybe Nil (uncurry Cons) . f

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f = cata . listAlgebra f

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f z = flip appEndo z . getDual . cata (listDualEndoAlgebra $ flip f)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z = \case
  [] -> z
  h:t -> let x = f z h in seq x $ foldl' f x t

length :: [a] -> Int
length = foldl' (flip ((+) . const 1)) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = go
  where
    go = embed . bimap f go . project

filter :: (a -> Bool) -> [a] -> [a]
filter p = cata f
  where
    f Nil = []
    f (Cons a b) = if p a then a:b else b

(++) :: [a] -> [a] -> [a]
(++) = flip (foldr (:))

concat :: [[a]] -> [a]
concat = foldr (++) []
