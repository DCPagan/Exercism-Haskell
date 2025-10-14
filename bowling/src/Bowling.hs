{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Bowling (score, BowlingError(..)) where

import Control.Applicative
import Control.Arrow (Arrow(..))
import Control.Monad
import Control.Monad.Codensity
import Control.Monad.Except

import Data.Ix
import Data.Monoid

data Frame where
  Standard :: Int -> Int -> Frame
  Spare :: Int -> Int -> Frame
  Strike :: Int -> Frame
  Final :: Int -> Int -> Frame
  FinalSpare :: Int -> Int -> Frame
  FinalStrike :: Int -> Int -> Frame
  FinalSpareStrike :: Int -> Frame
  FinalStrikeSpare :: Int -> Frame
  FinalTwoStrikes :: Int -> Frame
  FinalThreeStrikes :: Frame

instance Show Frame where
  show = \case
    Standard a b -> show a ++ ":" ++ show b
    Spare s a -> show s ++ '(':show a ++ "/)"
    Strike s -> show s ++ "(X)"
    Final a b -> show a ++ ":" ++ show b
    FinalSpare a b -> show a ++ "/" ++ show b
    FinalStrike a b -> 'X':show a ++ ":" ++ show b
    FinalSpareStrike a -> show a ++ "/X"
    FinalStrikeSpare a -> 'X':show a ++ "/"
    FinalTwoStrikes a -> "XX" ++ show a
    FinalThreeStrikes -> "XXX"
  showList = (++) . unwords . fmap show

data BowlingF a where
  Get :: ((Int, Int) -> BowlingF a) -> BowlingF a
  Fork :: ((Int, Int) -> BowlingF a) -> BowlingF a -> BowlingF a
  Peek :: ([Int] -> BowlingF a) -> BowlingF a
  Tell :: Endo [Frame] -> BowlingF a -> BowlingF a
  Pure :: a -> BowlingF a
  Fail :: BowlingError -> BowlingF a
  deriving (Functor)

instance (Show a) => Show (BowlingF a) where
  show = \case
    Get _ -> "Get"
    Fork _ m -> "Fork " ++ show m
    Peek _ -> "Peek"
    Tell f m -> "Tell " ++ show (appEndo f []) ++ ' ':show m
    Pure x -> show x
    Fail x -> show x

instance Applicative BowlingF where
  pure = Pure
  (<*>) = ap

instance Monad BowlingF where
  Get f >>= k = Get $ f >=> k
  Fork f m >>= k = fork (f >=> k) (m >>= k)
    where
      fork g = \case
        Get h -> Get $ liftA2 (<|>) g h
        Fork h n -> fork (liftA2 (<|>) g h) n
        Tell s n -> case fork g n of
          Tell t o -> fork g (Tell (s <> t) o)
          o -> Tell s o
        Fail _ -> Get g
        n -> Fork g n
  Peek f >>= k = Peek $ f >=> k
  Tell a x >>= k = case x >>= k of
    Tell b y -> Tell (a <> b) y
    y -> Tell a y
  Pure a >>= k = k a
  Fail x >>= _ = Fail x

instance Alternative BowlingF where
  empty = Fail IncompleteGame
  -- |Failure is the unit.
  Fail _ <|> x = x
  x <|> Fail _ = x

  -- |The score is delivered immediately.
  Tell a x <|> Tell b y = Tell (a <> b) (x <|> y)
  Tell a x <|> y = Tell a (x <|> y)
  x <|> Tell a y = Tell a (x <|> y)

  -- |Forked consumption
  Fork f (Fail _) <|> m = Get f <|> m
  m <|> Fork f (Fail _) = m <|> Get f
  Fork f m <|> Fork g n = Fork (liftA2 (<|>) f g) (m <|> n)
  Fork f m <|> Get g = Get (liftA2 (<|>) f g) <|> m
  Get f <|> Fork g m = Get (liftA2 (<|>) f g) <|> m
  Fork f m <|> n = Fork f $ m <|> n
  m <|> Fork f n = Fork f $ m <|> n

  Get f <|> Get g = Get $ liftA2 (<|>) f g
  Get f <|> m = Fork f m
  m <|> Get f = Fork f m

  Peek f <|> Peek g = Peek $ liftA2 (<|>) f g
  Peek f <|> x = Peek $ (<|> x) . f
  x <|> Peek f = Peek $ (x <|>) . f

  -- |Short-circuit pure values.
  Pure a <|> Pure _ = Pure a

instance MonadError BowlingError BowlingF where
  throwError = Fail
  catchError m k = case m of
    Fail e -> k e
    x -> x

instance (MonadError e m) => MonadError e (Codensity m) where
  throwError = lift . throwError
  catchError m k = lift $ catchError (lowerCodensity m) (lowerCodensity . k)

type Bowling = Codensity BowlingF

roll :: Bowling (Int, Int)
roll = Codensity Get

peek :: Bowling [Int]
peek = Codensity Peek

frame :: Frame -> Bowling ()
frame x = Codensity $ Tell (Endo (x:)) . ($ ())

incomplete :: Bowling a
incomplete = empty

rollMax :: Int -> Bowling (Int, Int)
rollMax x = do
  r@(rollIndex, rollValue) <- roll
  unless (inRange (0, x) rollValue) $ do
    throwError InvalidRoll { .. }
  return r

rollExact :: Int -> Bowling (Int, Int)
rollExact x = do
  r@(rollIndex, rollValue) <- roll
  unless (rollValue == x) $ do
    throwError InvalidRoll { .. }
  return r

strike :: Bowling ()
strike = do
  rollExact 10
  points <- (10 +) . sum . take 2 <$> peek
  frame $ Strike points

spare :: Bowling ()
spare = do
  rollValue <- snd <$> rollMax 9
  rollExact (10 - rollValue)
  points <- (10 +) . sum . take 1 <$> peek
  frame $ Spare points rollValue

standard :: Bowling ()
standard = do
  a <- snd <$> rollMax 9
  b <- snd <$> rollMax (9 - a)
  frame $ Standard a b

turn :: Bowling ()
turn = strike <|> spare <|> standard

finalThreeStrikes :: Bowling ()
finalThreeStrikes = do
  replicateM_ 3 $ rollExact 10
  frame FinalThreeStrikes

finalTwoStrikes :: Bowling ()
finalTwoStrikes = do
  replicateM_ 2 $ rollExact 10
  rollValue <- snd <$> rollMax 9
  frame $ FinalTwoStrikes rollValue

finalStrikeSpare :: Bowling ()
finalStrikeSpare = do
  rollExact 10
  rollValue <- snd <$> rollMax 9
  rollExact (10 - rollValue)
  frame $ FinalStrikeSpare rollValue

finalSpareStrike :: Bowling ()
finalSpareStrike = do
  rollValue <- snd <$> rollMax 9
  rollMax (10 - rollValue)
  rollExact 10
  frame $ FinalSpareStrike rollValue

finalStrike :: Bowling ()
finalStrike = do
  rollExact 10
  a <- snd <$> rollMax 9
  b <- snd <$> rollMax (9 - a)
  frame $ FinalStrike a b

finalSpare :: Bowling ()
finalSpare = do
  a <- snd <$> rollMax 9
  rollExact (10 - a)
  b <- snd <$> rollMax 9
  frame $ FinalSpare a b

finalStandard :: Bowling ()
finalStandard = do
  a <- snd <$> rollMax 9
  b <- snd <$> rollMax (9 - a)
  frame $ Final a b

finalTurn :: Bowling ()
finalTurn =
  finalThreeStrikes <|> finalTwoStrikes <|> finalStrikeSpare
  <|> finalSpareStrike <|> finalStrike <|> finalSpare <|> finalStandard

bowling :: Bowling ()
bowling = replicateM_ 9 turn >> finalTurn

-- |Parse the bowling game, returning the frames and the return value.
runBowlingF :: BowlingF a -> Int -> [Int] -> Either BowlingError ([Frame], a)
runBowlingF game rollIndex rolls = case game of
  Get f -> case rolls of
    [] -> Left IncompleteGame
    rollValue:rolls ->
      runBowlingF (f (rollIndex, rollValue)) (succ rollIndex) rolls
  Fork f m -> case rolls of
    [] -> runBowlingF m rollIndex []
    rolls -> case m of
      Get g -> runBowlingF (Get (liftA2 (<|>) f g)) rollIndex rolls
      Fork g n -> runBowlingF (Fork (liftA2 (<|>) f g) n) rollIndex rolls
      Peek g -> runBowlingF (Fork f $ g rolls) rollIndex rolls
      Tell s n -> runBowlingF (Tell s $ Fork f n) rollIndex rolls
      _ -> runBowlingF (Get f) rollIndex rolls
  Peek f -> runBowlingF (f rolls) rollIndex rolls
  Tell s x -> first (appEndo s) <$> runBowlingF x rollIndex rolls
  Pure x -> case rolls of
    [] -> Right ([], x)
    rollValue:_ -> Left $ InvalidRoll { .. }
  Fail x -> Left x

runBowling :: Bowling a -> Int -> [Int] -> Either BowlingError ([Frame], a)
runBowling = runBowlingF . lowerCodensity

-- |Parse the bowling game to frames.
execBowlingF :: BowlingF () -> Int -> [Int] -> Either BowlingError [Frame]
execBowlingF game rollIndex rolls = fst <$> runBowlingF game rollIndex rolls

execBowling :: Bowling () -> Int -> [Int] -> Either BowlingError [Frame]
execBowling = execBowlingF . lowerCodensity

-- |Evaluate the frames to a score.
frameScore :: Frame -> Int
frameScore = \case
  Standard a b -> a + b
  Spare s _ -> s
  Strike s -> s
  Final a b -> a + b
  FinalSpare _ b -> 10 + b
  FinalStrike a b -> 10 + a + b
  FinalSpareStrike _ -> 20
  FinalStrikeSpare _ -> 20
  FinalTwoStrikes a -> 20 + a
  FinalThreeStrikes -> 30

evalFrames :: [Frame] -> Int
evalFrames = getSum . foldMap (Sum . frameScore)

data BowlingError
  = IncompleteGame
  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score = fmap evalFrames . execBowling bowling 0
