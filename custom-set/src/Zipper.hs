{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Zipper
  ( Color(..)
  , _Red
  , _Black
  , Direction
  , RBZip(..)
  , direction
  , val
  , sibling
  , RBZipper
  , RBFocus(..)
  , RBFocusM
  , RBFocusM'
  , rbtree
  , zipper
  , _rbSibling
  , goLeft
  , goLeft'
  , goRight
  , goRight'
  , goUp
  , sweepLeft
  , sweepRight
  , makeFocus
  , extendZipper
  , unzipFocus
  , zipperSearch) where

import Control.Lens
import Control.Monad.State (MonadState, StateT(..), gets, modify)

import Data.Foldable (Foldable(..))
import Data.Function (fix)

import RedBlackTree

data Color
  = Red
  | Black
  deriving (Eq, Show, Enum, Bounded)

makePrisms ''Color

type Direction = Either Color Color

{-|
  Zipper over the red-black tree.
-}
data RBZipF a b where
  RBZipF :: { _directionF :: Direction, _valF :: a, _childF :: b }
    -> RBZipF a b
  deriving (Eq, Show, Functor, Foldable, Traversable)

type role RBZip nominal
data RBZip a where
  RBZip :: { _direction :: Direction, _val :: a, _sibling :: CustomSet a }
    -> RBZip a
  deriving (Show)

makeLenses ''RBZip

type RBZipper a = [RBZip a]

type role RBFocus nominal
data RBFocus a where
  RBFocus :: { _rbtree :: CustomSet a, _zipper :: RBZipper a } -> RBFocus a
  deriving (Show)

type RBFocusM a = StateT (RBFocus a)
class (MonadState (RBFocus a) m) => RBFocusM' a m

makeLenses ''RBFocus

_rbSibling :: Traversal' (RBFocus a) (CustomSet a)
_rbSibling = zipper . _head . sibling

instance Semigroup (RBFocus a) where
  x <> RBFocus { .. } = extendZipper' _zipper x

instance Monoid (RBFocus a) where
  mempty = RBFocus { _rbtree = RBNil, _zipper = [] }

extendZipper' :: RBZipper a -> RBFocus a -> RBFocus a
extendZipper' z = over zipper (<> z)

extendZipper :: (Monad m) => RBZipper a -> RBFocusM a m ()
extendZipper = modify . extendZipper'

makeFocus :: CustomSet a -> RBFocus a
makeFocus _rbtree = RBFocus { _zipper = [], .. }

goLeft' :: RBFocus a -> RBFocus a
goLeft' x@RBFocus { .. } = case _rbtree of
  RBNil -> x
  RNode RBNode { .. } -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Red) _tval _tright:_zipper }
  BNode RBNode { .. } -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Black) _tval _tright:_zipper }

goLeft :: (Monad m) => RBFocusM a m ()
goLeft = modify goLeft'

goRight' :: RBFocus a -> RBFocus a
goRight' x@RBFocus { .. } = case _rbtree of
  RBNil -> x
  RNode RBNode { .. } -> RBFocus
    { _rbtree = _tright, _zipper = RBZip (Right Red) _tval _tleft:_zipper }
  BNode RBNode { .. } -> RBFocus
    { _rbtree = _tright, _zipper = RBZip (Right Black) _tval _tleft:_zipper }

goRight :: (Monad m) => RBFocusM a m ()
goRight = modify goRight'

up :: CustomSet a -> RBZip a -> CustomSet a
up _tleft (RBZip (Left Red) _tval _tright) = RNode RBNode { .. }
up _tleft (RBZip (Left Black) _tval _tright) = BNode RBNode { .. }
up _tright (RBZip (Right Red) _tval _tleft) = RNode RBNode { .. }
up _tright (RBZip (Right Black) _tval _tleft) = BNode RBNode { .. }

goUp' :: RBFocus a -> RBFocus a
goUp' x@RBFocus {_zipper = []} = x
goUp' RBFocus {_zipper = z:_zipper, ..} = RBFocus { _rbtree = up _rbtree z, .. }

goUp :: (Monad m) => RBFocusM a m ()
goUp = modify goUp'

unzipFocus' :: RBFocus a -> CustomSet a
unzipFocus' RBFocus { .. } = foldl' up _rbtree _zipper

unzipFocus :: (Monad m) => RBFocusM a m (CustomSet a)
unzipFocus = gets unzipFocus'

sweepLeft' :: RBFocus a -> RBFocus a
sweepLeft' x@RBFocus { .. } = case _rbtree of
  RBNil -> x
  RNode RBNode { _tleft = RBNil } -> x
  BNode RBNode { _tleft = RBNil } -> x
  _ -> sweepLeft' $ goLeft' x

sweepRight' :: RBFocus a -> RBFocus a
sweepRight' x@RBFocus { .. } = case _rbtree of
  RBNil -> x
  RNode RBNode { _tright = RBNil } -> x
  BNode RBNode { _tright = RBNil } -> x
  _ -> sweepRight' $ goRight' x

sweepLeft :: (Monad m) => RBFocusM a m ()
sweepLeft = modify sweepLeft'

sweepRight :: (Monad m) => RBFocusM a m ()
sweepRight = modify sweepRight'

zipperSearch' :: (Ord a) => a -> RBFocus a -> RBFocus a
zipperSearch' x = fix f
  where
    f g focus@RBFocus { .. } = case _rbtree of
      RBNil -> focus
      RNode RBNode { .. } -> case compare x _tval of
        LT -> g
          RBFocus { _rbtree = _tleft
                  , _zipper = RBZip (Left Red) _tval _tright:_zipper
                  }
        EQ -> focus
        GT -> g
          RBFocus { _rbtree = _tright
                  , _zipper = RBZip (Right Red) _tval _tleft:_zipper
                  }
      BNode RBNode { .. } -> case compare x _tval of
        LT -> g
          RBFocus { _rbtree = _tleft
                  , _zipper = RBZip (Left Black) _tval _tright:_zipper
                  }
        EQ -> focus
        GT -> g
          RBFocus { _rbtree = _tright
                  , _zipper = RBZip (Right Black) _tval _tleft:_zipper
                  }

zipperSearch :: (Monad m, Ord a) => a -> RBFocusM a m ()
zipperSearch = modify . zipperSearch'
