module PerfectNumbers where

import Control.Monad
import Data.List

data Classification = Deficient | Perfect | Abundant
  deriving (Bounded, Enum, Eq, Show)

fromOrdering :: Ordering -> Classification
fromOrdering = toEnum . fromEnum

toRoot :: Integral a => a -> [a]
toRoot n = takeWhile (\x -> x * x <= n) (enumFromTo 2 n)

properDivisors :: Integral a => a -> Maybe [a]
properDivisors n
  | n <= 0 = Nothing
  | n == 1 = Just []
  | otherwise = Just $ 1 : do
    x <- toRoot n
    guard $ mod n x == 0
    nub [x, div n x]

aliquotSum :: Integral a => a -> Maybe a
aliquotSum = fmap sum . properDivisors

classify :: Int -> Maybe Classification
classify = fmap <$> (fromOrdering .) . flip compare <*> fmap sum . properDivisors
