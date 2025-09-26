{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module DND (Character(..), ability, modifier, character) where

import Control.Monad

import Data.List (sortBy)

import Test.QuickCheck (Arbitrary(..), Gen, elements)

data Character =
  Character
  { strength :: Int
  , dexterity :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom :: Int
  , charisma :: Int
  , hitpoints :: Int
  }
  deriving (Show, Eq)

dice :: Gen Int
dice = elements [1 .. 6]

modifier :: Int -> Int
modifier = flip div 2 . subtract 10

ability :: Gen Int
ability = sum . take 3 . sortBy (flip compare) <$> replicateM 4 dice

character :: Gen Character
character = do
  strength <- ability
  dexterity <- ability
  constitution <- ability
  intelligence <- ability
  wisdom <- ability
  charisma <- ability
  let hitpoints = 10 + modifier constitution
  return Character {..}

instance Arbitrary Character where
  arbitrary = character
