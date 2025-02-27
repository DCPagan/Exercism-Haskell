module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p = foldr (
  \a b -> if p a
    then b
    else a : b
  ) []

keep :: (a -> Bool) -> [a] -> [a]
keep p = foldr (
  \a b -> if p a
    then a : b
    else b
  ) []
