{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Recursion where

import Data.Kind (Type)
import Data.List (uncons)
import Control.Arrow (Arrow((&&&)))

{-|
  Recursion schemes
-}
type family Base f :: Type -> Type

class Functor (Base t) => Recursive t where
  project :: t -> Base t t

class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t

data ListF a b where
  Nil :: ListF a b
  Cons :: a -> b -> ListF a b

instance Functor (ListF a) where
  fmap f = \case
    Nil -> Nil
    Cons h t -> Cons h (f t)

type instance Base [a] = ListF a

instance Recursive [a] where
  project = maybe Nil (uncurry Cons) . uncons

instance Corecursive [a] where
  embed = \case
    Nil -> []
    Cons h t -> h:t

cata :: (Recursive t) => (Base t a -> a) -> t -> a
cata alg = go
  where
    go = alg . fmap go . project

ana :: (Corecursive t) => (a -> Base t a) -> a -> t
ana coalg = go
  where
    go = embed . fmap go . coalg

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = go
  where
    go = alg . fmap go . coalg

para :: (Recursive t) => (Base t (t, b) -> b) -> t -> b
para alg = go
  where
    go = alg . fmap (id &&& go) . project

zygo :: (Recursive t) => (Base t b -> b) -> (Base t (b, c) -> c) -> t -> c
zygo f g = snd . cata (f . fmap fst &&& g)
