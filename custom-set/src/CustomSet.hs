{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

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
  , union) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Lens.Extras
import Control.Monad
import Control.Monad.State (evalState, get)

import Data.Function (on)

import Prelude hiding (null)

import Recursion (cata)

import RedBlackTree
  ( CustomSet(..)
  , RBNode(..)
  , RBTreeF(..)
  , _BNode
  , _rbnode
  , isBlack
  , isRed
  , makeBlack
  , makeRed
  , rbLeft
  , rbRight
  , tleft
  , tleft
  , tright
  , tright
  , tval)

import Zipper

instance (Ord a) => Semigroup (CustomSet a) where
  (<>) = union

instance (Ord a) => Monoid (CustomSet a) where
  mempty = empty

member :: (Ord a) => a -> CustomSet a -> Bool
member x = go
  where
    go = \case
      RBNil -> False
      RNode s -> member' s
      BNode s -> member' s

    member' RBNode {..} = _tval == x || on (||) go _tleft _tright

empty :: CustomSet a
empty = RBNil

null :: CustomSet a -> Bool
null = \case
  RBNil -> True
  _ -> False

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

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x = makeBlack . evalState (del x) . makeFocus
  where
    del :: (Monad m, Ord a) => a -> RBFocusM a m (CustomSet a)
    del x = do
      zipperSearch x
      current@RBFocus {..} <- get
      case _rbtree of
        RBNil -> unzipFocus
        RNode n -> del' current n
        BNode n -> del' current n

    del' :: (Monad m, Ord a)
      => RBFocus a
      -> RBNode a (CustomSet a)
      -> RBFocusM a m (CustomSet a)
    del' RBFocus {..} RBNode {..} = do
      deleted <- if null _tleft
        then do
          rbtree .= _tright
          return _rbtree
        else if null _tright
          then do
            rbtree .= _tleft
            return _rbtree
          else do
            goRight
            sweepLeft
            down <- use rbtree
            case down ^? _rbnode . chosen of
              Nothing -> return down
              Just RBNode {..} -> do
                zipper . _last . val .= _tval
                extendZipper _zipper
                rbtree .= _tright
                return down
      if is _BNode deleted || null deleted
        then balance
        else unzipFocus

    {-|
      Based on `RB-DELETE-FIXUP` from the CLRS chapter which treats red-black
      trees, except with a zipper instead of parent references, and monadic
      lenses instead of updating pointers all the time.
    -}
    balance :: (Monad m) => RBFocusM a m (CustomSet a)
    balance = do
      RBFocus {..} <- get
      if isRed _rbtree
        then 
          -- |Red node: change the color to black and exit.
          rbtree %= makeBlack >> unzipFocus
        else case _zipper ^? _head of
          -- |Top of the tree.
          Nothing -> unzipFocus
          Just RBZip {..} -> case _direction of
            Left _ -> do
              when (isRed _sibling) $ do
                goUp
                rbtree %= (rbRight %~ makeBlack >>> makeRed >>> leftRotate)
                goLeft
              rightSibling <- use $ rbtree . singular rbRight
              if isBlack (rightSibling ^. singular rbLeft) && isBlack
                (rightSibling ^. singular rbRight)
                then do
                  rbtree . rbRight %= makeRed
                  balance
                else do
                  when (isBlack $ rightSibling ^. singular rbRight) $ do
                    rbtree . rbRight
                      %= (rbLeft %~ makeBlack >>> makeRed >>> rightRotate)
                  tree <- use rbtree
                  let color =
                        if isRed tree
                          then makeRed
                          else makeBlack
                  rbtree %= (rbRight %~ color >>> rbRight . rbRight %~ makeBlack
                             >>> leftRotate)
                  unzipFocus
            Right _ -> do
              when (isRed _sibling) $ do
                goUp
                rbtree %= (rbLeft %~ makeBlack >>> makeRed >>> rightRotate)
                goRight
              leftSibling <- use $ rbtree . singular rbLeft
              if isBlack (leftSibling ^. singular rbRight) && isBlack
                (leftSibling ^. singular rbLeft)
                then do
                  rbtree . rbLeft %= makeRed
                  balance
                else do
                  when (isBlack $ leftSibling ^. singular rbLeft) $ do
                    rbtree . rbLeft
                      %= (rbRight %~ makeBlack >>> makeRed >>> leftRotate)
                  tree <- use rbtree
                  let color =
                        if isRed tree
                          then makeRed
                          else makeBlack
                  rbtree %= (rbLeft %~ color >>> rbLeft . rbLeft %~ makeBlack
                             >>> rightRotate)
                  unzipFocus

    leftRotate :: CustomSet a -> CustomSet a
    leftRotate = \case
      RBNil -> RBNil
      RNode x@RBNode {_tright = RBNil} -> RNode x
      RNode x@RBNode {_tright = RNode y@RBNode {..}} ->
        RNode y { _tleft = RNode x { _tright = _tleft } }
      RNode x@RBNode {_tright = BNode y@RBNode {..}} ->
        BNode y { _tleft = RNode x { _tright = _tleft } }
      BNode x@RBNode {_tright = RBNil} -> RNode x
      BNode x@RBNode {_tright = RNode y@RBNode {..}} ->
        RNode y { _tleft = BNode x { _tright = _tleft } }
      BNode x@RBNode {_tright = BNode y@RBNode {..}} ->
        BNode y { _tleft = BNode x { _tright = _tleft } }

    rightRotate :: CustomSet a -> CustomSet a
    rightRotate = \case
      RBNil -> RBNil
      RNode y@RBNode {_tleft = RBNil} -> RNode y
      RNode y@RBNode {_tleft = RNode x@RBNode {..}} ->
        RNode x { _tright = RNode y { _tleft = _tright } }
      RNode y@RBNode {_tleft = BNode x@RBNode {..}} ->
        BNode x { _tright = RNode y { _tleft = _tright } }
      BNode y@RBNode {_tleft = RBNil} -> RNode y
      BNode y@RBNode {_tleft = RNode x@RBNode {..}} ->
        RNode x { _tright = BNode y { _tleft = _tright } }
      BNode y@RBNode {_tleft = BNode x@RBNode {..}} ->
        BNode x { _tright = BNode y { _tleft = _tright } }

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert empty

toList :: (Ord a) => CustomSet a -> [a]
toList = cata $ \case
  RBNilF -> []
  RNodeF n -> cat n
  BNodeF n -> cat n
  where
    cat RBNode {..} = _tleft ++ _tval:_tright

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union RBNil x = x
union x RBNil = x
union _setA _setB = undefined

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection _setA _setB = undefined

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference _setA _setB = undefined

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom setA setB = null $ intersection setA setB

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf _setA _setB = undefined
