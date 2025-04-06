{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Change (findFewestCoins) where

import Control.Monad.Memo
import Data.Function
import Data.Maybe
import Data.List.Extra

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins = listToMaybe $ startEvalMemo
  $ findFewestCoins' target coins

findFewestCoins' :: Integer -> [Integer]
  -> Memo (Integer, [Integer]) [[Integer]] [[Integer]]
findFewestCoins' 0 _ = return [[]]
findFewestCoins' _ [] = return []
findFewestCoins' target coins@(c:cs) =
  if target < c
  then return []
  else do
    x <- fmap (c:) <$> for2 memo findFewestCoins' (target - c) coins
    y <- for2 memo findFewestCoins' target cs
    return $ headDef [] $ groupOn length $ mergeBy (on compare length) x y
