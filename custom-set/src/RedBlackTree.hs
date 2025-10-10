{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OrPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module RedBlackTree where

import Control.Arrow (Arrow((&&&), (***)))
import Control.Lens hiding (para)

import Data.Biapplicative (Biapplicative(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bifunctor.TH
import Data.Function (on)
import Data.Functor.Classes (Eq1(..), Eq2(..))
import Data.Kind (Type)
import Data.List (intercalate, uncons)
import Data.Monoid (All(..), Endo(..))

import GHC.Generics

import Numeric.Natural (Natural)

import Recursion

{-|
  Red-black tree
  cf. [C. Okasaki, “Some Familiar Data Structures in a Functional Setting,” in Purely Functional Data Structures, Cambridge: Cambridge University Press, 1998, pp. 24–29](https://doi.org/10.1017/CBO9780511530104)
  cf. [C. Okasaki, “Red-black trees in a functional setting,” Journal of Functional Programming, vol. 9, no. 4, pp. 471–477, 1999. doi:10.1017/S0956796899003494](https://doi.org/10.1017/S0956796899003494)
-}
data RBNode a b where
  RBNode :: { _tleft :: b, _tval :: a, _tright :: b } -> RBNode a b
  deriving (Eq, Functor, Foldable, Generic, Traversable)

makeLenses ''RBNode

deriveBifunctor ''RBNode

deriveBifoldable ''RBNode

deriveBitraversable ''RBNode

instance Biapplicative RBNode where
  bipure a b = RBNode { _tleft = b, _tval = a, _tright = b }
  biliftA2
    f
    g
    RBNode {_tleft = al, _tval = av, _tright = ar}
    RBNode {_tleft = bl, _tval = bv, _tright = br} =
    RBNode { _tleft = g al bl, _tval = f av bv, _tright = g ar br }

instance Eq2 RBNode where
  liftEq2 f g a b = getAll $ bifoldMap All All $ biliftA2 f g a b

instance (Eq a) => Eq1 (RBNode a) where
  liftEq = liftEq2 (==)

instance (Show a, Show b) => Show (RBNode a b) where
  show RBNode {..} =
    "[" ++ show _tleft ++ "," ++ show _tval ++ "," ++ show _tright ++ "]"

data RBTreeF a b where
  RBNilF :: RBTreeF a b
  RNodeF :: RBNode a b -> RBTreeF a b
  BNodeF :: RBNode a b -> RBTreeF a b
  deriving (Functor, Foldable, Generic, Traversable)

makePrisms ''RBTreeF

deriveBifunctor ''RBTreeF

deriveBifoldable ''RBTreeF

deriveBitraversable ''RBTreeF

_rbnodeF :: Prism' (RBTreeF c d) (Either (RBNode c d) (RBNode c d))
_rbnodeF = prism' (either RNodeF BNodeF) $ \case
  RBNilF -> Nothing
  RNodeF n -> Just $ Left n
  BNodeF n -> Just $ Right n

{-|
  Biapplicative interface lifts functions by zipping
-}
instance Biapplicative RBTreeF where
  bipure a b = RNodeF $ RBNode { _tleft = b, _tval = a, _tright = b }
  biliftA2 f g (RNodeF a) (RNodeF b) = RNodeF $ biliftA2 f g a b
  biliftA2 f g (BNodeF a) (BNodeF b) = BNodeF $ biliftA2 f g a b
  biliftA2 _ _ _ _ = RBNilF

instance Eq2 RBTreeF where
  liftEq2 _ _ RBNilF RBNilF = True
  liftEq2 f g (RNodeF a) (RNodeF b) = liftEq2 f g a b
  liftEq2 f g (BNodeF a) (BNodeF b) = liftEq2 f g a b
  liftEq2 _ _ _ _ = False

instance (Eq a) => Eq1 (RBTreeF a) where
  liftEq = liftEq2 (==)

instance (Eq a, Eq b) => Eq (RBTreeF a b) where
  (==) = liftEq (==)

data CustomSet a where
  RBNil :: CustomSet a
  RNode :: RBNode a (CustomSet a) -> CustomSet a
  BNode :: RBNode a (CustomSet a) -> CustomSet a
  deriving Generic

makePrisms ''CustomSet

_rbnode :: Prism'
    (CustomSet a)
    (Either (RBNode a (CustomSet a)) (RBNode a (CustomSet a)))
_rbnode = prism' (either RNode BNode) $ \case
  RBNil -> Nothing
  RNode n -> Just $ Left n
  BNode n -> Just $ Right n

_rbBlack :: Prism' (CustomSet a) (Maybe (RBNode a (CustomSet a)))
_rbBlack = prism' (maybe RBNil BNode) $ \case
  RBNil -> Just Nothing
  RNode _ -> Nothing
  BNode x -> Just $ Just x

rbLeft :: Traversal' (CustomSet b) (CustomSet b)
rbLeft = _rbnode . chosen . tleft

rbRight :: Traversal' (CustomSet b) (CustomSet b)
rbRight = _rbnode . chosen . tright

type instance Base (CustomSet a) = RBTreeF a

instance Recursive (CustomSet a) where
  project = \case
    RBNil -> RBNilF
    RNode a -> RNodeF a
    BNode a -> BNodeF a

instance Corecursive (CustomSet a) where
  embed = \case
    RBNilF -> RBNil
    RNodeF a -> RNode a
    BNodeF a -> BNode a

instance (Show a, Show b) => Show (RBTreeF a b) where
  show = \case
    RBNilF -> "nil"
    RNodeF RBNode {..} ->
      "R(" ++ show _tleft ++ "," ++ show _tval ++ "," ++ show _tright ++ ")"
    BNodeF RBNode {..} ->
      "B(" ++ show _tleft ++ "," ++ show _tval ++ "," ++ show _tright ++ ")"

instance (Show a) => Show (CustomSet a) where
  show = cata format
    where
      format :: (Show a) => RBTreeF a String -> String
      format = \case
        RBNilF -> "nil"
        RNodeF x -> "R" ++ formatNode x
        BNodeF x -> "B" ++ formatNode x
        where
          formatNode RBNode {..} =
            "[" ++ intercalate "," [_tleft, show _tval, _tright] ++ "]"

{-|
  Only strictly increasing functions can be mapped over an ordered structure.
-}
mapMonotone :: (Ord a, Ord b) => (a -> b) -> CustomSet a -> CustomSet b
mapMonotone f = go
  where
    go = embed . bimap f go . project

{-|
  Null nodes are black.
-}
isRed :: CustomSet a -> Bool
isRed RNode {} = True
isRed _ = False

isBlack :: CustomSet a -> Bool
isBlack RNode {} = False
isBlack _ = True

makeBlack :: CustomSet a -> CustomSet a
makeBlack = \case
  RBNil -> RBNil
  RNode n -> BNode n
  BNode n -> BNode n

makeRed :: CustomSet a -> CustomSet a
makeRed = \case
  RBNil -> RBNil
  RNode n -> RNode n
  BNode n -> RNode n

blackHeight :: CustomSet a -> Natural
blackHeight = cata $ \case
  RBNilF -> 1
  RNodeF RBNode {..} -> max _tleft _tright
  BNodeF RBNode {..} -> succ $ max _tleft _tright

{-|
  The elements are ordered.
-}
orderRule :: (Ord a) => CustomSet a -> Bool
orderRule = zygo minMax testOrder
  where
    minMax :: RBTreeF a (Maybe (a, a)) -> Maybe (a, a)
    minMax = \case
      RBNilF -> Nothing
      RNodeF n -> minMax' n
      BNodeF n -> minMax' n
      where
        minMax' :: RBNode a (Maybe (a, a)) -> Maybe (a, a)
        minMax'
          RBNode {..} = Just (maybe _tval fst _tleft, maybe _tval snd _tright)

    testOrder :: (Ord a) => RBTreeF a (Maybe (a, a), Bool) -> Bool
    testOrder = \case
      RBNilF -> True
      RNodeF x -> testNode x
      BNodeF x -> testNode x
      where
        testNode :: (Ord a) => RBNode a (Maybe (a, a), Bool) -> Bool
        testNode RBNode {..} = liftEq2 isOrdered (&&) _tleft _tright
          where
            isOrdered a b =
              maybe True ((<= _tval) . snd) a
              && maybe True ((_tval <=) . fst) b

{-|
  No red node has a red child.
-}
redRule :: CustomSet a -> Bool
redRule = para $ \case
  RBNilF -> True
  RNodeF (RBNode {..}) -> on (&&) isBlackChild _tleft _tright
  _ -> True
  where
    isBlackChild = bifoldr (flip (&&) . isBlack) const True

{-|
  Every path from the root to an empty node has the same number of black nodes.
-}
blackRule :: CustomSet a -> Bool
blackRule = zygo (count @Natural) testBlack
  where
    count :: (Integral b) => RBTreeF a b -> b
    count = \case
      RBNilF -> 1
      RNodeF RBNode {..} -> max _tleft _tright
      BNodeF RBNode {..} -> succ $ max _tleft _tright

    testBlack :: (Integral b) => RBTreeF a (b, Bool) -> Bool
    testBlack = \case
      RBNilF -> True
      RNodeF x -> testNode x
      BNodeF x -> testNode x
      where
        testNode RBNode {..} = liftEq2 (==) (&&) _tleft _tright

{-|
  The length of the longest path precedes double that of the shortest.
-}
heightRule :: CustomSet a -> Bool
heightRule = zygo (heights @Natural) testHeights
  where
    getHeights :: (Integral b, Ord b) => RBNode a (b, b) -> (b, b)
    getHeights RBNode {..} = succ *** succ $ biliftA2 min max _tleft _tright

    heights :: (Integral b, Ord b) => RBTreeF a (b, b) -> (b, b)
    heights = \case
      RBNilF -> (1, 1)
      RNodeF x -> getHeights x
      BNodeF x -> getHeights x

    testNode :: (Integral b, Ord b) => RBNode a ((b, b), Bool) -> Bool
    testNode = uncurry (>=) . first (2 *) . getHeights . second fst

    testHeights :: (Integral b, Ord b) => RBTreeF a ((b, b), Bool) -> Bool
    testHeights = \case
      RBNilF -> True
      RNodeF x -> testNode x
      BNodeF x -> testNode x
