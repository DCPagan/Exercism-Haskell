{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable (for_)
import Data.Function (on)

import ResistorColors (Color(..), value)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , arbitraryBoundedEnum
  , elements
  , forAll)

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

main :: IO ()
main =
  hspecWith
    defaultConfig { configFailFast = True
                  }
    specs

specs :: Spec
specs = describe "value" $ do
  describe "equality tests" $ for_ cases test

  describe "property tests" $ do
    it "all values starting with Black are single digit" $
      forAll arbitrary $ \color -> value (Black, color) < 10
    it "Monotone" $
      forAll arbitrary $ \(a, b) -> compare a b == on compare value a b
    it "Nine divides differece of transpositions" $
      forAll arbitrary $ \(a, b) -> mod (value (a, b) - value (b, a)) 9 == 0
    it "Eleven divides sum of transpositions" $
      forAll arbitrary $ \(a, b) -> mod (value (a, b) + value (b, a)) 11 == 0
  where
    -- Add more property tests here
    test Case {..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion = value input `shouldBe` expected

data Case =
  Case
  { description :: String
  , input :: (Color, Color)
  , expected :: Int
  }

cases :: [Case]
cases =
  [ Case { description = "Brown and black"
         , input = (Brown, Black)
         , expected = 10
         }
  , Case { description = "Blue and grey"
         , input = (Blue, Grey)
         , expected = 68
         }
  , Case { description = "Yellow and violet"
         , input = (Yellow, Violet)
         , expected = 47
         }
  , Case { description = "Orange and orange"
         , input = (Orange, Orange)
         , expected = 33
         }
    -- Note: This test suite omits testing three-color bands,
    -- since they are not representable as (Color, Color). They
    -- are addressed in the exercise resistor-color-trio.
  ]
