{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar
  ( Day
  , DayOfWeek
  , addDays
  , dayOfWeek
  , dayOfWeekDiff
  , fromGregorian)

data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Enum, Bounded, Show)

data Schedule
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth
  deriving (Eq, Ord, Enum, Bounded, Show)

toDayOfWeek :: Weekday -> DayOfWeek
toDayOfWeek = toEnum . fromEnum

toWeekday :: DayOfWeek -> Weekday
toWeekday = toEnum . fromEnum

toNextDayOfWeek :: DayOfWeek -> Day -> Day
toNextDayOfWeek w = fromIntegral . dayOfWeekDiff w . dayOfWeek >>= addDays

toPriorDayOfWeek :: DayOfWeek -> Day -> Day
toPriorDayOfWeek
  w = negate . fromIntegral . flip dayOfWeekDiff w . dayOfWeek >>= addDays

firstDayOfWeek :: Weekday -> Integer -> Int -> Day
firstDayOfWeek w y m = toNextDayOfWeek (toDayOfWeek w) (fromGregorian y m 1)

lastDayOfWeek :: Weekday -> Integer -> Int -> Day
lastDayOfWeek w y m = toPriorDayOfWeek (toDayOfWeek w) (fromGregorian y m 31)

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month = case schedule of
  First -> firstDayOfWeek weekday year month
  Second -> addDays 7 $ firstDayOfWeek weekday year month
  Third -> addDays 14 $ firstDayOfWeek weekday year month
  Fourth -> addDays 21 $ firstDayOfWeek weekday year month
  Last -> lastDayOfWeek weekday year month
  Teenth -> toNextDayOfWeek (toDayOfWeek weekday) $ fromGregorian year month 13
