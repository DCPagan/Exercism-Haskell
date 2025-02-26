module CollatzConjecture (collatz) where

import Data.List
import Data.Maybe

step :: Integral a => a -> a
step n = if even n then div n 2 else 3 * n + 1

collatz :: Integer -> Maybe Integer
collatz n = if n <= 0
  then Nothing
  else Just . genericLength . unfoldr (\i ->
    if i == 1
    then Nothing
    else Just (i, step i)) $ n
