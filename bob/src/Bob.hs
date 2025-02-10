{-# LANGUAGE MultiWayIf #-}

module Bob (responseFor) where

import Data.Char
import Data.Functor.Contravariant
import Data.List

isQuestion :: String -> Bool
isQuestion = ('?' ==) . last . dropWhileEnd isSpace

isYelling :: String -> Bool
isYelling = getPredicate (Predicate (all isUpper) <> Predicate (not . null))
  . filter isLetter

isSilent :: String -> Bool
isSilent = all isSpace

responseFor :: String -> String
responseFor xs = if
  | isSilent xs -> "Fine. Be that way!"
  | isQuestion xs -> if
    | isYelling xs -> "Calm down, I know what I'm doing!"
    | otherwise -> "Sure."
  | isYelling xs -> "Whoa, chill out!"
  | otherwise -> "Whatever."
