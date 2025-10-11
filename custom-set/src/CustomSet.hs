{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import Test.QuickCheck (Arbitrary(..), conjoin, recursivelyShrink)

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

    member' RBNode { .. } = _tval == x || on (||) go _tleft _tright

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

    ins' n@RBNode { .. } = case compare v _tval of
      LT -> tleft %~ ins $ n
      EQ -> n
      GT -> tright %~ ins $ n

    {-|
      Balance a black node per Okasaki.
      n.b.: This method is only to be called on black nodes.
     -}
    balance :: RBNode a (CustomSet a) -> CustomSet a
    balance
      z@RBNode
        { _tleft = RNode y@RBNode
          { _tleft = RNode x@RBNode
            { _tright = a }
          , _tright = b } } =
      RNode y
        { _tleft = BNode x { _tright = a }
        , _tright = BNode z { _tleft = b } }
    balance
      z@RBNode
        { _tleft = RNode x@RBNode
          { _tright = RNode y@RBNode
            { _tleft = a
            , _tright = b } } } =
      RNode y
        { _tleft = BNode x { _tright = a }
        , _tright = BNode z { _tleft = b } }
    balance
      x@RBNode
        { _tright = RNode z@RBNode
          { _tleft = RNode y@RBNode
            { _tleft = a
            , _tright = b } } } =
      RNode y
        { _tleft = BNode x { _tright = a }
        , _tright = BNode z { _tleft = b } }
    balance
      x@RBNode
        { _tright = RNode y@RBNode
          { _tright = RNode z@RBNode
            { _tleft = b }
          , _tleft = a } } =
      RNode y
        { _tleft = BNode x { _tright = a }
        , _tright = BNode z { _tleft = b } }
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
    nodeSize RBNode { .. } = _tleft + _tright + 1

leftRotate :: CustomSet a -> CustomSet a
leftRotate = \case
  RBNil -> RBNil
  RNode x@RBNode { _tright = RBNil } -> RNode x
  RNode x@RBNode { _tright = RNode y@RBNode { .. } } ->
    RNode y { _tleft = RNode x { _tright = _tleft } }
  RNode x@RBNode { _tright = BNode y@RBNode { .. } } ->
    BNode y { _tleft = RNode x { _tright = _tleft } }
  BNode x@RBNode { _tright = RBNil } -> BNode x
  BNode x@RBNode { _tright = RNode y@RBNode { .. } } ->
    RNode y { _tleft = BNode x { _tright = _tleft } }
  BNode x@RBNode { _tright = BNode y@RBNode { .. } } ->
    BNode y { _tleft = BNode x { _tright = _tleft } }

rightRotate :: CustomSet a -> CustomSet a
rightRotate = \case
  RBNil -> RBNil
  RNode y@RBNode { _tleft = RBNil } -> RNode y
  RNode y@RBNode { _tleft = RNode x@RBNode { .. } } ->
    RNode x { _tright = RNode y { _tleft = _tright } }
  RNode y@RBNode { _tleft = BNode x@RBNode { .. } } ->
    BNode x { _tright = RNode y { _tleft = _tright } }
  BNode y@RBNode { _tleft = RBNil } -> BNode y
  BNode y@RBNode { _tleft = RNode x@RBNode { .. } } ->
    RNode x { _tright = BNode y { _tleft = _tright } }
  BNode y@RBNode { _tleft = BNode x@RBNode { .. } } ->
    BNode x { _tright = BNode y { _tleft = _tright } }

delete :: (Ord a) => a -> CustomSet a -> CustomSet a
delete x = makeBlack . evalState (del x) . makeFocus
  where
    del :: (Monad m, Ord a) => a -> RBFocusM a m (CustomSet a)
    del x = do
      zipperSearch x
      current@RBFocus { .. } <- get
      case _rbtree of
        RBNil -> unzipFocus
        RNode n -> del' current n
        BNode n -> del' current n

    del' :: (Monad m, Ord a)
      => RBFocus a
      -> RBNode a (CustomSet a)
      -> RBFocusM a m (CustomSet a)
    del' RBFocus { .. } RBNode { .. } = do
      deleted <- if
        | null _tleft -> do
          rbtree .= _tright
          return _rbtree
        | null _tright -> do
          rbtree .= _tleft
          return _rbtree
        | otherwise -> do
          goRight
          sweepLeft
          delendum <- use rbtree
          let RBNode { .. } = delendum ^. singular _rbnode . chosen
          zipper . _last . val .= _tval
          rbtree .= _tright
          extendZipper _zipper
          return delendum
      if isBlack deleted
        then balance
        else unzipFocus

    {-|
      Based on `RB-DELETE-FIXUP` from the CLRS chapter which treats red-black
      trees, except with a zipper instead of parent references, monadic lenses
      instead of updating pointers all the time, and tail recursion instead of
      a while loop.
    -}
    balance :: (Monad m) => RBFocusM a m (CustomSet a)
    balance = do
      focus <- get
      case focus of
        -- |Red node: change the color to black and exit.
        RBFocus { _rbtree = RNode _ } -> rbtree %= makeBlack >> unzipFocus
        -- |Top of the tree.
        RBFocus { _zipper = [], .. } -> return _rbtree
        RBFocus { _zipper = RBZip { .. }:_, .. } -> goUp >> case _direction of
          Left _ -> do
            when (isRed _sibling) $ do
              rbtree %= (rbRight %~ makeBlack >>> makeRed >>> leftRotate)
              goLeft
            sibling' <- use $ rbtree . singular rbRight
            if has (rbLeft . filtered isBlack) sibling' &&
              has (rbRight . filtered isBlack) sibling'
              then do
                rbtree . rbRight %= makeRed
                balance
              else do
                when (has (rbRight . filtered isBlack) sibling') $
                  rbtree . rbRight
                    %= (rbLeft %~ makeBlack >>> makeRed >>> rightRotate)
                tree <- use rbtree
                let color = if isRed tree then makeRed else makeBlack
                rbtree %=
                  (makeBlack
                    >>> rbRight %~ color
                    >>> rbRight . rbRight %~ makeBlack
                    >>> leftRotate)
                unzipFocus
          Right _ -> do
            when (isRed _sibling) $ do
              rbtree %= (rbLeft %~ makeBlack >>> makeRed >>> rightRotate)
              goRight
            sibling' <- use $ rbtree . singular rbLeft
            if has (rbLeft . filtered isBlack) sibling' &&
              has (rbRight . filtered isBlack) sibling'
              then do
                rbtree . rbLeft %= makeRed
                balance
              else do
                when (has (rbLeft . filtered isBlack) sibling') $
                  rbtree . rbLeft
                    %= (rbRight %~ makeBlack >>> makeRed >>> leftRotate)
                tree <- use rbtree
                let color = if isRed tree then makeRed else makeBlack
                rbtree %=
                  (makeBlack
                    >>> rbLeft %~ color
                    >>> rbLeft . rbLeft %~ makeBlack
                    >>> rightRotate)
                unzipFocus

fromList :: (Ord a) => [a] -> CustomSet a
fromList = foldr insert empty

toList :: (Ord a) => CustomSet a -> [a]
toList = cata $ \case
  RBNilF -> []
  RNodeF n -> cat n
  BNodeF n -> cat n
  where
    cat RBNode { .. } = _tleft ++ _tval:_tright

union :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union RBNil setA = setA
union setB RBNil = setB
union setA setB = fromList $ on unionList toList setA setB
  where
    unionList :: (Ord a) => [a] -> [a] -> [a]
    unionList [] b = b
    unionList a [] = a
    unionList (a:aa) (b:bb) = case compare a b of
      LT -> a:unionList aa (b:bb)
      EQ -> a:unionList aa bb
      GT -> b:unionList (a:aa) bb

intersection :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection RBNil _ = empty
intersection _ RBNil = empty
intersection setA setB = fromList $ on intersectionList toList setA setB
  where
    intersectionList :: (Ord a) => [a] -> [a] -> [a]
    intersectionList [] _ = []
    intersectionList _ [] = []
    intersectionList (a:aa) (b:bb) = case compare a b of
      LT -> intersectionList aa (b:bb)
      EQ -> a:intersectionList aa bb
      GT -> intersectionList (a:aa) bb

difference :: (Ord a) => CustomSet a -> CustomSet a -> CustomSet a
difference setA setB = fromList $ on differenceList toList setA setB
  where
    differenceList :: (Ord a) => [a] -> [a] -> [a]
    differenceList [] _ = []
    differenceList a [] = a
    differenceList (a:aa) (b:bb) = case compare a b of
      LT -> a:differenceList aa (b:bb)
      EQ -> differenceList aa bb
      GT -> b:differenceList (a:aa) bb

isDisjointFrom :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom = on isDisjointFromList toList
  where
    isDisjointFromList :: (Ord a) => [a] -> [a] -> Bool
    isDisjointFromList [] _ = True
    isDisjointFromList _ [] = True
    isDisjointFromList (a:aa) (b:bb) = case compare a b of
      LT -> isDisjointFromList aa (b:bb)
      EQ -> False
      GT -> isDisjointFromList (a:aa) bb

isSubsetOf :: (Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf RBNil _ = True
isSubsetOf _ RBNil = False
isSubsetOf setA setB = on isSubsetOfList toList setA setB
  where
    isSubsetOfList :: (Ord a) => [a] -> [a] -> Bool
    isSubsetOfList [] _ = True
    isSubsetOfList _ [] = False
    isSubsetOfList (a:aa) (b:bb) = case compare a b of
      LT -> False
      EQ -> isSubsetOfList aa bb
      GT -> isSubsetOfList (a:aa) bb

instance (Eq a, Ord a) => Eq (CustomSet a) where
  (==) = on (==) toList

deriving instance (Eq a, Ord a) => Eq (RBZip a)
deriving instance (Eq a, Ord a) => Eq (RBFocus a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (RBNode a b) where
  arbitrary = do
    _tleft <- arbitrary
    _tval <- arbitrary
    _tright <- arbitrary
    return RBNode { .. }

instance (Arbitrary a, Ord a) => Arbitrary (CustomSet a) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap makeBlack . recursivelyShrink
