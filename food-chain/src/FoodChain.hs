{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module FoodChain (song) where

import Control.Lens
import Data.Functor.Foldable
import Data.List
import qualified Data.List.NonEmpty as L

data Verse where
  Verse ::
    { _animal :: String
    , _comment :: Maybe String
    , _misc :: Maybe String
    , _isDead :: Bool
    } -> Verse
  deriving (Eq, Show)
makeLenses ''Verse

type Stanza = [Verse]

verses :: [Verse]
verses =
  [ Verse "fly" Nothing Nothing False
  , Verse
    "spider"
    (Just "It wriggled and jiggled and tickled inside her.")
    (Just "that wriggled and jiggled and tickled inside her")
    False
  , Verse "bird" (Just "How absurd to swallow a bird!") Nothing False
  , Verse "cat" (Just "Imagine that, to swallow a cat!") Nothing False
  , Verse "dog" (Just "What a hog, to swallow a dog!") Nothing False
  , Verse
    "goat"
    (Just "Just opened her throat and swallowed a goat!")
    Nothing
    False
  , Verse "cow" (Just "I don't know how she swallowed a cow!") Nothing False
  , Verse "horse" Nothing Nothing True
  ]

toStanzas :: [Verse] -> [Stanza]
toStanzas = fmap reverse . L.tail . L.inits

formatPreamble :: Verse -> String
formatPreamble Verse { .. } =
  "I know an old lady who swallowed a " ++ _animal ++ ".\n"
  ++ (if _isDead
    then "She's dead, of course!\n"
    else maybe "" (++ "\n") _comment)

formatSubsequent :: [Verse] -> String
formatSubsequent = zygo verseAlg songAlg
  where
    verseAlg = \case
      Nil -> Nothing
      Cons verse _ -> Just verse
    songAlg = \case
      Nil -> ""
      Cons Verse { _animal } (nextVerse, rest) ->
        (maybe (
          "I don't know why she swallowed the " ++ _animal
          ++ ". Perhaps she'll die.\n"
          ) (\Verse { _animal = next, _misc } ->
            "She swallowed the " ++ _animal ++ " to catch the " ++ next
            ++ maybe "" (' ':) _misc ++ ".\n"
          ) $ nextVerse
        ) ++ rest

formatStanza :: Stanza -> String
formatStanza = \case
  [] -> ""
  stanza@(preamble@Verse { .. } : _) -> formatPreamble preamble
    ++ if _isDead
      then ""
      else formatSubsequent stanza

song :: String
song = intercalate "\n" $ fmap formatStanza $ toStanzas verses

{-
song :: String
song =
    "I know an old lady who swallowed a fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a spider.\n\
    \It wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a bird.\n\
    \How absurd to swallow a bird!\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cat.\n\
    \Imagine that, to swallow a cat!\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a dog.\n\
    \What a hog, to swallow a dog!\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a goat.\n\
    \Just opened her throat and swallowed a goat!\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a cow.\n\
    \I don't know how she swallowed a cow!\n\
    \She swallowed the cow to catch the goat.\n\
    \She swallowed the goat to catch the dog.\n\
    \She swallowed the dog to catch the cat.\n\
    \She swallowed the cat to catch the bird.\n\
    \She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.\n\
    \She swallowed the spider to catch the fly.\n\
    \I don't know why she swallowed the fly. Perhaps she'll die.\n\
    \\n\
    \I know an old lady who swallowed a horse.\n\
    \She's dead, of course!\n"
-}
