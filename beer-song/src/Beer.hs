{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Beer (song) where

import Data.Type.Bool
import GHC.TypeLits
import Data.Data (Proxy(..))

type a :**: b = AppendSymbol a (ConsSymbol ' ' b)
type a :++: b = AppendSymbol a (ConsSymbol '\n' b)
type a :--: b = AppendSymbol a (AppendSymbol "\n\n" b)

type ShowChar (a :: Char) = ConsSymbol a ""

type family UnfixNat (n :: Nat) :: Maybe Nat where
  UnfixNat 0 = Nothing
  UnfixNat n = Just (n - 1)

type family FixNat (n :: Maybe Nat) :: Nat where
  FixNat Nothing = 0
  FixNat (Just n) = n + 1

type family ShowDigit (n :: Nat) :: Symbol where
  ShowDigit n = ShowChar (NatToChar (Mod n 10 + CharToNat '0'))

type family ShowNat' (n :: Nat) :: Symbol where
  ShowNat' 0 = ""
  ShowNat' n = AppendSymbol (ShowNat' (Div n 10)) (ShowDigit n)

type family ShowNat (n :: Nat) :: Symbol where
  ShowNat 0 = "0"
  ShowNat n = ShowNat' n

infixr 7 :**:
infixr 6 :++:
infixr 5 :--:

type family Bottle' (n :: Nat) :: Symbol where
  Bottle' 0 = "No more bottles"
  Bottle' 1 = "1 bottle"
  Bottle' n = ShowNat n :**: "bottles"

type family Bottle (n :: Nat) :: Symbol where
  Bottle 0 = "no more bottles"
  Bottle 1 = "1 bottle"
  Bottle n = ShowNat n :**: "bottles"

type family Take (n :: Nat) :: Symbol where
  Take 1 = "Take it down"
  Take _ = "Take one down"

type family Verse (n :: Nat) :: Symbol where
  Verse n =
    Bottle' n :**: "of beer on the wall," :**: Bottle n :**: "of beer." :++:
    Take n :**: "and pass it around," :**:
    Bottle (n - 1) :**: "of beer on the wall."

type family Coda (n :: Nat) :: Symbol where
  Coda n = 
    Bottle' 0 :**: "of beer on the wall," :**: Bottle 0 :**: "of beer." :++:
    "Go to the store and buy some more," :**:
    Bottle n :**: "of beer on the wall.\n"

type family Song' (m :: Nat) (n :: Maybe Nat) :: Symbol where
  Song' m Nothing = Coda m
  Song' m (Just n) = Verse (n + 1) :--: Song' m (UnfixNat n)

type family Song (n :: Nat) :: Symbol where
  Song n = Song' n (UnfixNat n)

type BeerSong = Song 99

song :: String
song = symbolVal $ Proxy @BeerSong
