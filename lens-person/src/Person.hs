{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Person
  ( Address(..)
  , Born(..)
  , Name(..)
  , Person(..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet) where

import Control.Lens

import Data.Time.Calendar
  ( Day
  , DayOfMonth
  , MonthOfYear
  , Year
  , fromGregorian
  , toGregorian)
import Data.Tuple.Extra (uncurry3)

data Address =
  Address
  { _street :: String
  , _houseNumber :: Int
  , _place :: String
  , _country :: String
  }

makeLenses ''Address

data Name = Name { _foreNames :: String, _surName :: String }

makeLenses ''Name

data Born = Born { _bornAt :: Address, _bornOn :: Day }

makeLenses ''Born

data Person = Person { _name :: Name, _born :: Born, _address :: Address }

makeLenses ''Person

gregorian :: Iso' Day (Year, MonthOfYear, DayOfMonth)
gregorian = iso toGregorian $ uncurry3 fromGregorian

bornStreet :: Born -> String
bornStreet = view $ bornAt . street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set $ address . street

setBirthMonth :: Int -> Person -> Person
setBirthMonth = set $ born . bornOn . gregorian . _2

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (address . street) f . over (born . bornAt . street) f
