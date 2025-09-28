{-# LANGUAGE DataKinds #-}
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

type family (a :: Symbol) :**: (b :: Symbol) where
  "" :**: b = b
  a :**: "" = a
  a :**: b = AppendSymbol a (ConsSymbol ' ' b)

infixr 6 :**:

type family (a :: Symbol) :++: (b :: Symbol) where
  "" :++: b = b
  a :++: "" = a
  a :++: b = AppendSymbol a (ConsSymbol '\n' b)

infixr 5 :++:

type family Subject (n :: Nat) where
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
  Subject _ = TypeError (Text "Invalid verse index")

type family Apposition (n :: Nat) where
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
  Apposition _ = TypeError (Text "Invalid verse index")

type family Subordinate (n :: Nat) where
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
  Subordinate _ = TypeError (Text "Invalid verse index")

type Preamble = "This is"

type Coda = ".\n"

type family Unnatural (n :: Natural) where
  Unnatural 0 = 'Nothing
  Unnatural n = 'Just (n - 1)

type family Verse (n :: Natural) where
  Verse n = Subject n :**: Apposition n :++: Subordinate n

type family Verses' (n :: Maybe Natural) where
  Verses' 'Nothing = ""
  Verses' ('Just n) = Verse n :**: Verses' (Unnatural n)

type family Stanza' (n :: Maybe Natural) where
  Stanza' 'Nothing = ""
  Stanza' n = Preamble :**: AppendSymbol (Verses' n) Coda

type family Song' (n :: Maybe Natural) where
  Song' 'Nothing = ""
  Song' ('Just n) = Song' (Unnatural n) :++: Stanza' ('Just n) 

type SongLength = 12

class (KnownNat n, n <= SongLength) => VerseIndex n where
  type Verses (n :: Natural) :: Symbol
  type Verses n = Verses' (Unnatural n)

  type Stanza (n :: Natural) :: Symbol
  type Stanza n = Stanza' (Unnatural n)

  type Song (n :: Natural) :: Symbol
  type Song n = Song' (Unnatural n)

type TheSong = Song' (Unnatural SongLength)

rhyme :: String
rhyme = symbolVal $ Proxy @TheSong

rhyme' :: String
rhyme' =
  "This is the house that Jack built.\n\
        \\n\
        \This is the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n\
        \\n\
        \This is the horse and the hound and the horn\n\
        \that belonged to the farmer sowing his corn\n\
        \that kept the rooster that crowed in the morn\n\
        \that woke the priest all shaven and shorn\n\
        \that married the man all tattered and torn\n\
        \that kissed the maiden all forlorn\n\
        \that milked the cow with the crumpled horn\n\
        \that tossed the dog\n\
        \that worried the cat\n\
        \that killed the rat\n\
        \that ate the malt\n\
        \that lay in the house that Jack built.\n"
