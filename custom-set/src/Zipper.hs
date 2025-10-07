{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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
  , child
  , RBZipper
  , RBFocus
  , rbtree
  , zipper
  , _rbSibling
  , goLeft
  , goRight
  , goUp
  , sweepLeft
  , sweepRight
  , unzipFocus) where

import Control.Lens

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
data RBZip a where
  RBZip :: { _direction :: Direction, _val :: a, _child :: CustomSet a }
    -> RBZip a
  deriving (Eq, Show)

makeLenses ''RBZip

type RBZipper a = [RBZip a]

data RBFocus a where
  RBFocus :: { _rbtree :: CustomSet a, _zipper :: RBZipper a } -> RBFocus a
  deriving (Eq, Show)

makeLenses ''RBFocus

_rbSibling :: Traversal' (RBFocus a) (CustomSet a)
_rbSibling = zipper . _head . child

goLeft :: RBFocus a -> RBFocus a
goLeft x@RBFocus {..} = case _rbtree of
  RBNil -> x
  RNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Red) _tval _tright:_zipper }
  BNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Black) _tval _tright:_zipper }

goRight :: RBFocus a -> RBFocus a
goRight x@RBFocus {..} = case _rbtree of
  RBNil -> x
  RNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Right Red) _tval _tright:_zipper }
  BNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Right Black) _tval _tright:_zipper }

goUp :: RBFocus a -> RBFocus a
goUp x@RBFocus {_zipper = []} = x
goUp
  RBFocus {_rbtree = _tleft, _zipper = RBZip (Left Red) _tval _tright:_zipper} =
  RBFocus { _rbtree = RNode RBNode { .. }, .. }
goUp
  RBFocus {_rbtree = _tleft,
           _zipper = RBZip (Left Black) _tval _tright:_zipper} =
  RBFocus { _rbtree = BNode RBNode { .. }, .. }
goUp
  RBFocus {_rbtree = _tright, _zipper = RBZip (Right Red) _tval _tleft:_zipper} =
  RBFocus { _rbtree = RNode RBNode { .. }, .. }
goUp
  RBFocus {_rbtree = _tright,
           _zipper = RBZip (Right Black) _tval _tleft:_zipper} =
  RBFocus { _rbtree = BNode RBNode { .. }, .. }

unzipFocus :: RBFocus a -> CustomSet a
unzipFocus RBFocus {_zipper = [], ..} = _rbtree
unzipFocus
  RBFocus {_rbtree = _tleft, _zipper = RBZip (Left Red) _tval _tright:_zipper} =
  unzipFocus $ RBFocus { _rbtree = RNode RBNode { .. }, .. }
unzipFocus
  RBFocus {_rbtree = _tleft,
           _zipper = RBZip (Left Black) _tval _tright:_zipper} =
  unzipFocus $ RBFocus { _rbtree = BNode RBNode { .. }, .. }
unzipFocus
  RBFocus {_rbtree = _tright, _zipper = RBZip (Right Red) _tval _tleft:_zipper} =
  unzipFocus $ RBFocus { _rbtree = RNode RBNode { .. }, .. }
unzipFocus
  RBFocus {_rbtree = _tright,
           _zipper = RBZip (Right Black) _tval _tleft:_zipper} =
  unzipFocus $ RBFocus { _rbtree = BNode RBNode { .. }, .. }

sweepLeft :: RBFocus a -> RBFocus a
sweepLeft x@RBFocus {..} = case _rbtree of
  RBNil -> x
  RNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Red) _tval _tright:_zipper }
  BNode RBNode {..} -> RBFocus
    { _rbtree = _tleft, _zipper = RBZip (Left Black) _tval _tright:_zipper }

sweepRight :: RBFocus a -> RBFocus a
sweepRight x@RBFocus {..} = case _rbtree of
  RBNil -> x
  RNode RBNode {..} -> RBFocus
    { _rbtree = _tright, _zipper = RBZip (Right Red) _tval _tleft:_zipper }
  BNode RBNode {..} -> RBFocus
    { _rbtree = _tright, _zipper = RBZip (Right Black) _tval _tleft:_zipper }

extendFocus :: RBZipper a -> RBFocus a -> RBFocus a
extendFocus z = over zipper (<> z)

zipperSearch :: (Ord a) => a -> CustomSet a -> RBFocus a
zipperSearch x _rbtree = fix f RBFocus { _zipper = [], .. }
  where
    f g focus@RBFocus {..} = case _rbtree of
      RBNil -> focus
      RNode RBNode {..} -> case compare x _tval of
        LT -> g
          RBFocus { _rbtree = _tleft
                  , _zipper = RBZip (Left Red) _tval _tright:_zipper
                  }
        EQ -> focus
        GT -> g
          RBFocus { _rbtree = _tright
                  , _zipper = RBZip (Right Red) _tval _tleft:_zipper
                  }
      BNode RBNode {..} -> case compare x _tval of
        LT -> g
          RBFocus { _rbtree = _tleft
                  , _zipper = RBZip (Left Black) _tval _tright:_zipper
                  }
        EQ -> focus
        GT -> g
          RBFocus { _rbtree = _tright
                  , _zipper = RBZip (Right Black) _tval _tleft:_zipper
                  }
