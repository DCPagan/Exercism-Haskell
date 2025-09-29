{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Proverb (recite) where

import Control.Applicative (liftA2)
import Data.List (intercalate, uncons)

proverb :: String -> String -> String
proverb a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."

proverbs :: [String] -> String
proverbs = intercalate "\n" . (zipWith proverb <*> tail)

coda' :: String -> String
coda' a = "And all for the want of a " ++ a ++ "."

coda :: [String] -> String
coda = maybe "" (coda' . fst) . uncons

joinLines :: String -> String -> String
joinLines "" b = b
joinLines a "" = a
joinLines a b = a ++ '\n':b

recite :: [String] -> String
recite = liftA2 joinLines proverbs coda
