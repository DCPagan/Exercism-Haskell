{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module LinkedList
  ( LinkedList
  , datum
  , fromList
  , isNil
  , new
  , next
  , nil
  , reverseLinkedList
  , toList) where

import Data.Bifunctor (Bifunctor(..))
import Data.Fix
import Data.Functor.Classes (Eq1(..), Eq2(..))

data LinkedListF a b where
  LLNilF :: LinkedListF a b
  LLConsF :: a -> b -> LinkedListF a b
  deriving (Eq, Functor, Show)

instance Eq2 LinkedListF where
  liftEq2 _ _ LLNilF LLNilF = True
  liftEq2 _ _ LLNilF _ = False
  liftEq2 _ _ _ LLNilF = False
  liftEq2 f g (LLConsF a b) (LLConsF c d) = f a c && g b d

instance (Eq a) => Eq1 (LinkedListF a) where
  liftEq = liftEq2 (==)

instance Bifunctor LinkedListF where
  bimap f g = \case
    LLNilF -> LLNilF
    LLConsF a b -> LLConsF (f a) (g b)
  first f = \case
    LLNilF -> LLNilF
    LLConsF a b -> LLConsF (f a) b
  second g = \case
    LLNilF -> LLNilF
    LLConsF a b -> LLConsF a (g b)

newtype LinkedList a = LinkedList { getLinkedList :: Fix (LinkedListF a) }
  deriving (Eq)

instance Functor LinkedList where
  fmap f = LinkedList . go . getLinkedList
    where
      go = Fix . bimap f go . unFix

instance (Show a) => Show (LinkedList a) where
  show l = "[" ++ foldFix show' (getLinkedList l) ++ "]"
    where
      show' = \case
        LLNilF -> ""
        LLConsF a b -> if null b
          then show a
          else show a ++ ',':b

fromListF :: [a] -> LinkedListF a [a]
fromListF = \case
  [] -> LLNilF
  h:t -> LLConsF h t

toListF :: LinkedListF a [a] -> [a]
toListF = \case
  LLNilF -> []
  LLConsF h t -> h:t

fromList :: [a] -> LinkedList a
fromList = LinkedList . refold Fix fromListF

toList :: LinkedList a -> [a]
toList = refold toListF unFix . getLinkedList

datum :: LinkedList a -> a
datum (LinkedList (Fix l)) = case l of
  LLNilF -> error "empty list"
  LLConsF h _ -> h

isNil :: LinkedList a -> Bool
isNil (LinkedList (Fix l)) = case l of
  LLNilF -> True
  _ -> False

new :: a -> LinkedList a -> LinkedList a
new h t = LinkedList $ Fix $ LLConsF h $ getLinkedList t

next :: LinkedList a -> LinkedList a
next (LinkedList (Fix l)) = case l of
  LLNilF -> error "empty list"
  LLConsF _ t -> LinkedList t

nil :: LinkedList a
nil = LinkedList $ Fix LLNilF

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = ($ nil) . foldFix alg . getLinkedList
  where
    alg = \case
      LLNilF -> id
      LLConsF a b -> b . new a
