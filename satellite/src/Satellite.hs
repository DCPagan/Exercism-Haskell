module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Control.Monad
import Data.List

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] _ = Nothing
treeFromTraversals preorder' inorder' = tree' preorder' inorder'
  where
    tree' [] [] = return Leaf
    tree' preorder inorder = do
      guard $ length (group $ sort preorder) == length preorder
      guard $ length (group $ sort inorder) == length inorder
      (root, rest) <- uncons preorder
      i <- elemIndex root inorder
      let (preLeft, preRight) = splitAt i rest
      let (inLeft, inRightRoot) = splitAt i inorder
      (_, inRight) <- uncons inRightRoot
      leftBranch <- tree' preLeft inLeft
      rightBranch <- tree' preRight inRight
      return $ Branch leftBranch root rightBranch
