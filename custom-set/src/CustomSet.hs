{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module CustomSet
  ( CustomSet(..)
  , delete
  , difference
  , empty
  , fromList
  , insert
  , intersection
  , isDisjointFrom
  , isSubsetOf
  , member
  , null
  , size
  , toList
  , union
    -- Red-Black tree properties
  , orderRule
  , redRule
  , blackRule
  , heightRule) where

import Control.Lens ((%~))

import Data.Function (on)

import Prelude hiding (null)

import Recursion (cata)

import RedBlackTree
  ( CustomSet(..)
  , RBNode(..)
  , RBTreeF(..)
  , blackRule
  , heightRule
  , orderRule
  , redRule
  , tleft
  , tright)

import Zipper ()

member :: (Ord a) => a -> CustomSet a -> Bool
member x = go
  where
    go = \case
      RBNil -> False
      RNode s -> member' s
      BNode s -> member' s
    member' RBNode {..} = _tval == x || on (||) go _tleft _tright

makeBlack :: CustomSet a -> CustomSet a
makeBlack = \case
  RBNil -> RBNil
  RNode n -> BNode n
  BNode n -> BNode n

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert v = makeBlack . ins
  where
    ins = \case
      RBNil -> RNode RBNode { _tleft = empty, _tval = v, _tright = empty }
      RNode n -> RNode $ ins' n
      BNode n -> balance $ ins' n
    ins' n@RBNode {..} = case compare v _tval of
      LT -> tleft %~ ins $ n
      EQ -> n
      GT -> tright %~ ins $ n
    {-|
      Implemented via or-pattern syntax, supported in GHC since v9.12.1.
      cf. [Or-Patterns](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/or_patterns.html)
    balance' :: RBNode x (CustomSet x) -> CustomSet x
    balance' (
      z@RBNode
        { _tleft = RNode y@RBNode
          { _tleft = RNode x@RBNode { _tright = b }
          , _tright = c } }
      z@RBNode
        { _tleft = RNode x@RBNode
          { _tright = RNode y@RBNode
            { _tleft = b
            , _tright = c } } }
      x@RBNode
        { _tright = RNode z@RBNode
          { _tleft = RNode y@RBNode
            { _tleft = b
            , _tright = c } } }
      x@RBNode
        { _tright = RNode y@RBNode
          { _tright = RNode z@RBNode { _tleft = c } }
        , _tleft = b }
      ) =
      RNode y
        { _tleft = BNode x { _tright = b }
        , _tright = BNode z { _tleft = c } }
    balance' x = BNode x
    -}
    {-|
      Balance a black node.
      n.b.: This method is only to be called on black nodes.
     -}
    balance :: RBNode a (CustomSet a) -> CustomSet a
    balance
      z@RBNode {_tleft = RNode
                  y@RBNode {_tleft = RNode x@RBNode {_tright = b}, _tright = c}} =
      RNode
        y
        { _tleft = BNode x { _tright = b }, _tright = BNode z { _tleft = c } }
    balance
      z@RBNode {_tleft = RNode
                  x@RBNode {_tright = RNode y@RBNode {_tleft = b, _tright = c}}} =
      RNode
        y
        { _tleft = BNode x { _tright = b }, _tright = BNode z { _tleft = c } }
    balance
      x@RBNode {_tright = RNode
                  z@RBNode {_tleft = RNode y@RBNode {_tleft = b, _tright = c}}} =
      RNode
        y
        { _tleft = BNode x { _tright = b }, _tright = BNode z { _tleft = c } }
    balance
      x@RBNode
      {_tright = RNode y@RBNode {_tright = RNode z@RBNode {_tleft = c}},
       _tleft = b} =
      RNode
        y
        { _tleft = BNode x { _tright = b }, _tright = BNode z { _tleft = c } }
    balance x = BNode x

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert empty

toList :: (Ord a) => CustomSet a -> [a]
toList = cata $ \case
  RBNilF -> []
  RNodeF n -> cat n
  BNodeF n -> cat n
  where
    cat RBNode {..} = _tleft ++ _tval:_tright

empty :: CustomSet a
empty = RBNil

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x = makeBlack . del
  where
    del = \case
      RBNil -> RBNil
      RNode n -> RNode $ del' n
      BNode n -> balance $ del' n
    del' n@RBNode {..} = case compare x _tval of
      LT -> tleft %~ del $ n
      EQ -> sweep' n
      GT -> tright %~ del $ n
    sweep' _n@RBNode {_tleft = RBNil} = undefined
    sweep' _n@RBNode {_tright = RBNil} = undefined
    sweep' _n@RBNode {} = undefined
    balance = undefined

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference _setA _setB = undefined

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection _setA _setB = undefined

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom _setA _setB = undefined

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf _setA _setB = undefined

null :: CustomSet a -> Bool
null = \case
  RBNil -> True
  _ -> False

size :: CustomSet a -> Int
size = cata size'
  where
    size' :: (Integral b) => RBTreeF a b -> b
    size' = \case
      RBNilF -> 0
      RNodeF x -> nodeSize x
      BNodeF x -> nodeSize x
    nodeSize :: (Integral b) => RBNode a b -> b
    nodeSize RBNode {..} = _tleft + _tright + 1

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union _setA _setB = undefined
