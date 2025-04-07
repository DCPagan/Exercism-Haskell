{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Change (findFewestCoins) where

import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.Functor
import Data.Function
import Data.List.Extra
import Data.Maybe

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise = listToMaybe $ runSTArray (makeChange target coins) ! target

makeChange :: MArray a [[Integer]] m
  => Integer -> [Integer] -> m (a Integer [[Integer]])
makeChange target coins = do
  z <- newArray (0, target) []
  foldrM f z (sort coins)
  where
    f :: MArray a [[Integer]] m
      => Integer -> a Integer [[Integer]] -> m (a Integer [[Integer]])
    f coin a =
      (getAssocs a >>= mapM_ (\(i, e) -> g i e >>= writeArray a i)) $> a
      where
        g 0 _ = return [[]]
        g total withoutCoin =
          if total < coin
          then return withoutCoin
          else do
            withCoin <- fmap (coin:) <$> readArray a (total - coin)
            return $ headDef [] $ groupOn length
              $ mergeBy (on compare length) withCoin withoutCoin
