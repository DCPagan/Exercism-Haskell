{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module POV (fromPOV, tracePathBetween) where

import Control.Arrow ((<<<), Arrow(..))

import Data.List (inits, tails, uncons)
import Data.Maybe (mapMaybe)
import Data.Monoid (Alt(..))
import Data.Tree (Forest, Tree(..))

data Zip a where
  Zip :: { _parentLabel :: a
         , _leftSiblings :: Forest a
         , _rightSiblings :: Forest a
         } -> Zip a
  deriving (Eq, Show)

type Zipper a = [Zip a]

data Focus a where
  Focus :: { _tree :: Tree a, _zipper :: Zipper a } -> Focus a
  deriving (Eq, Show)

unassoc :: (a, (b, c)) -> (a, b, c)
unassoc (a, (b, c)) = (a, b, c)

splits :: [a] -> [([a], a, [a])]
splits =
  mapMaybe (fmap unassoc . traverse uncons) . uncurry zip <<< inits &&& tails

toFocus :: Tree a -> Focus a
toFocus _tree = Focus { _zipper = [], .. }

focusChildren :: Focus a -> [Focus a]
focusChildren Focus { _tree = Node { .. }, .. } = focus <$> splits subForest
  where
    focus (_leftSiblings, _tree, _rightSiblings) =
      Focus { _zipper = Zip { _parentLabel = rootLabel, .. } : _zipper, .. }

goUp :: Focus a -> Focus a
goUp f@Focus { .. } = case _zipper of
  [] -> f
  Zip { .. } : _zipper -> Focus
    { _tree = Node { rootLabel = _parentLabel
                   , subForest = _leftSiblings ++ _tree : _rightSiblings
                   }
    , ..
    }

focusPOV :: Focus a -> Tree a
focusPOV focus@Focus { .. } = case _zipper of
  [] -> _tree
  Zip { .. } : _ -> case _tree of
    Node { .. } -> Node { subForest = subForest ++ [focusPOV up], .. }
    where
      up = case goUp focus of
        f@Focus { _tree = _tree@Node { rootLabel } } -> let
          subForest = _leftSiblings ++ _rightSiblings
          in
            f { _tree = Node { .. } }

zipperSearch :: Eq a => a -> Focus a -> Maybe (Focus a)
zipperSearch x focus@Focus { .. }
  | rootLabel _tree == x = Just focus
  | null $ subForest _tree = Nothing
  | otherwise = getAlt $ foldMap (Alt . zipperSearch x) $ focusChildren focus

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x = fmap focusPOV . zipperSearch x . toFocus

pathFromRoot :: Focus a -> [a]
pathFromRoot Focus { .. } =
  reverse $ rootLabel _tree : (_parentLabel <$> _zipper)

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  pov <- fromPOV from tree
  focus <- zipperSearch to $ toFocus pov
  return $ pathFromRoot focus
