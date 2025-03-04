{-# LANGUAGE LambdaCase #-}
module WordCount where

import Control.Applicative
import Control.Applicative.Combinators
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Coerce
import Data.Maybe
import Data.Monoid
import Data.List
import qualified Data.List.NonEmpty as L
import Text.ParserCombinators.ReadP (
  ReadP, char, eof, readP_to_S, satisfy, (<++))

isWordChar :: Char -> Bool
isWordChar = coerce $ Any . isAlphaNum <> Any . ('\'' ==)

isWordBreak :: Char -> Bool
isWordBreak = not . isWordChar

isQuote :: Char -> Bool
isQuote = \case
  '\'' -> True
  '"' -> True
  _ -> False

readUnquotedWord :: ReadP String
readUnquotedWord = some $ satisfy isWordChar

readQuotedWord :: ReadP String
readQuotedWord = between (char '\'') (char '\'') readUnquotedWord
  <|> between (char '"') (char '"') readUnquotedWord

readWord :: ReadP String
readWord = readQuotedWord <++ readUnquotedWord

readWordBreak :: ReadP Char
readWordBreak = satisfy isWordBreak

readWords :: ReadP [String]
readWords = between (many readWordBreak) (many readWordBreak)
  (sepBy readWord (some readWordBreak)) <* eof

readInput :: String -> [String]
readInput = fromMaybe []
  . (listToMaybe . readP_to_S readWords
    >=> (<$) <$> fmap (fmap toLower) . fst <*> guard . null . snd)

wordCount :: String -> [(String, Int)]
wordCount = fmap (L.head &&& L.length) . L.group . sort . readInput
