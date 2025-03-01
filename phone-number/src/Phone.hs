{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Phone (number) where

import Control.Monad
import Text.ParserCombinators.ReadP (ReadP, munch)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readMaybe, readPrec)
import Text.Regex.TDFA

data PhoneNumber where
  NANP :: {
    areaCode :: String,
    exchangeCode :: String,
    subscriberNumber :: String
  } -> PhoneNumber
  deriving (Eq, Ord)

nanpRegex :: String
nanpRegex = "^[[:space:][:punct:]]*(\\+?1)?[[:space:][:punct:]]*"
  ++ "([2-9][[:digit:]]{2})[[:space:][:punct:]]*"
  ++ "([2-9][[:digit:]]{2})[[:space:][:punct:]]*"
  ++ "([[:digit:]]{4})[[:space:][:punct:]]*$"

readNanp :: ReadP PhoneNumber
readNanp = do
  s <- munch $ const True
  let (_, _, _, m) = s =~ nanpRegex :: (String, String, String, [String])
  guard $ length m == 4
  let [_, areaCode, exchangeCode, subscriberNumber] = m
  return $ NANP {..}

instance Read PhoneNumber where
  readPrec = lift readNanp

instance Show PhoneNumber where
  show NANP {..} = areaCode ++ exchangeCode ++ subscriberNumber

readPhoneNumber :: String -> Maybe PhoneNumber
readPhoneNumber = readMaybe

number :: String -> Maybe String
number = fmap show . readPhoneNumber
