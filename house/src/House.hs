{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module House (rhyme) where

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeLits
import Data.Void
import Data.Proxy (Proxy(..))

type family (a :: Symbol) :**: (b :: Symbol) :: Symbol where
  "" :**: b = b
  a :**: "" = a
  a :**: b = AppendSymbol a (ConsSymbol ' ' b)

infixr 6 :**:

type family (a :: Symbol) :++: (b :: Symbol) :: Symbol where
  "" :++: b = b
  a :++: "" = a
  a :++: b = AppendSymbol a (ConsSymbol '\n' b)

infixr 5 :++:

type family Subject (n :: Natural) :: Symbol where
  Subject 0 = "the house"
  Subject 1 = "the malt"
  Subject 2 = "the rat"
  Subject 3 = "the cat"
  Subject 4 = "the dog"
  Subject 5 = "the cow"
  Subject 6 = "the maiden"
  Subject 7 = "the man"
  Subject 8 = "the priest"
  Subject 9 = "the rooster"
  Subject 10 = "the farmer"
  Subject 11 = "the horse"

type family Apposition (n :: Natural) :: Symbol where
  Apposition 0 = "that Jack built"
  Apposition 1 = ""
  Apposition 2 = ""
  Apposition 3 = ""
  Apposition 4 = ""
  Apposition 5 = "with the crumpled horn"
  Apposition 6 = "all forlorn"
  Apposition 7 = "all tattered and torn"
  Apposition 8 = "all shaven and shorn"
  Apposition 9 = "that crowed in the morn"
  Apposition 10 = "sowing his corn"
  Apposition 11 = "and the hound and the horn"

type family Subordinate (n :: Natural) :: Symbol where
  Subordinate 0 = ""
  Subordinate 1 = "that lay in"
  Subordinate 2 = "that ate"
  Subordinate 3 = "that killed"
  Subordinate 4 = "that worried"
  Subordinate 5 = "that tossed"
  Subordinate 6 = "that milked"
  Subordinate 7 = "that kissed"
  Subordinate 8 = "that married"
  Subordinate 9 = "that woke"
  Subordinate 10 = "that kept"
  Subordinate 11 = "that belonged to"

type Preamble = "This is"

type Coda = ".\n"

type family Unnatural (n :: Natural) :: Maybe Natural where
  Unnatural 0 = Nothing
  Unnatural n = Just (n - 1)

type family Verse (n :: Natural) :: Symbol where
  Verse n = Subject n :**: Apposition n :++: Subordinate n

type family Verses' (n :: Maybe Natural) :: Symbol where
  Verses' Nothing = ""
  Verses' (Just n) = Verse n :**: Verses' (Unnatural n)

type family Stanza' (n :: Maybe Natural) :: Symbol where
  Stanza' Nothing = ""
  Stanza' n = Preamble :**: AppendSymbol (Verses' n) Coda

type family Song' (n :: Maybe Natural) :: Symbol where
  Song' Nothing = ""
  Song' (Just n) = Song' (Unnatural n) :++: Stanza' (Just n) 

type SongLength :: Natural
type SongLength = 12

class (KnownNat n) => VerseIndex (n :: Natural) where
  type Verses (n :: Natural) :: Symbol
  type Stanza (n :: Natural) :: Symbol
  type Song (n :: Natural) :: Symbol

instance (KnownNat n, n <= SongLength) => VerseIndex n where
  type Verses n = If (n <=? SongLength)
    (Verses' (Unnatural n))
    (TypeError (Text "Invalid verse count; must precede 12."))
  type Stanza n = If (n <=? SongLength)
    (Stanza' (Unnatural n))
    (TypeError (Text "Invalid stanza length; must precede 12."))
  type Song n = If (n <=? SongLength)
    (Song' (Unnatural n))
    (TypeError (Text "Invalid song length; must precede 12."))

type TheSong :: Symbol
type TheSong = Song SongLength

rhyme :: String
rhyme = symbolVal $ Proxy @TheSong
