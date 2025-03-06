{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Robot where

import Control.Applicative
import Control.Monad
import Control.Monad.Codensity
import Control.Lens
import Text.ParserCombinators.ReadP (ReadP, get)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readPrec)

type Coord = (Integer, Integer)

data Bearing
  = North
  | East
  | South
  | West
  deriving (Bounded, Enum, Eq, Show, Ord)

_turnRight :: Bearing -> Bearing
_turnRight = \case
  North -> East
  East -> South
  South -> West
  West -> North

_turnLeft :: Bearing -> Bearing
_turnLeft = \case
  North -> West
  East -> North
  South -> East
  West -> South

go :: Bearing -> Coord -> Coord
go = \case
  North -> _2 %~ succ
  East -> _1 %~ succ
  South -> _2 %~ pred
  West -> _1 %~ pred

_advance :: Robot -> Robot
_advance Robot {..} = Robot { _coord = go _direction _coord, .. }

data Robot where
  Robot :: {
    _coord :: Coord,
    _direction :: Bearing
  } -> Robot
  deriving (Eq, Show, Ord)
makeLenses ''Robot

data InstructionF a where
  Stop :: InstructionF a
  Pure :: a -> InstructionF a
  TurnRight :: (Robot -> InstructionF a) -> InstructionF a
  TurnLeft :: (Robot -> InstructionF a) -> InstructionF a
  Advance :: (Robot -> InstructionF a) -> InstructionF a
  deriving Functor

instance Applicative InstructionF where
  pure = Pure
  (<*>) = ap

instance Alternative InstructionF where
  empty = Stop
  x <|> y = x *> y

instance Monad InstructionF where
  Stop >>= _ = Stop
  Pure x >>= f = f x
  TurnRight f >>= k = TurnRight $ f >=> k
  TurnLeft  f >>= k = TurnLeft $ f >=> k
  Advance f >>= k = Advance $ f >=> k

instance MonadFail InstructionF where
  fail = const Stop

instance MonadPlus InstructionF

type Instruction = Codensity InstructionF Robot

instance Semigroup Instruction where
  (<>) = (*>)

instance Monoid Instruction where
  mempty = stop

toInstruction :: Char -> Instruction
toInstruction = \case
  'R' -> turnRight
  'L' -> turnLeft
  'A' -> advance
  _ -> stop

stop :: Instruction
stop = Codensity $ const Stop

turnRight :: Instruction
turnRight = Codensity TurnRight

turnLeft :: Instruction
turnLeft = Codensity TurnLeft

advance :: Instruction
advance = Codensity Advance

readInstruction :: ReadP Instruction
readInstruction = toInstruction <$> get

readInstructions :: ReadP Instruction
readInstructions = mconcat <$> many readInstruction

instance Read Instruction where
  readPrec = lift $ readInstructions

dummy :: Robot
dummy = mkRobot North (0, 0)

formatInstr :: Show a => InstructionF a -> String
formatInstr Stop = ""
formatInstr (Pure x) = "(" ++ show x ++ ")"
formatInstr (TurnRight f) = 'R' : formatInstr (f dummy)
formatInstr (TurnLeft f) = 'L' : formatInstr (f dummy)
formatInstr (Advance f) = 'A' : formatInstr (f dummy)

instance Show a => Show (Codensity InstructionF a) where
  show = formatInstr . lowerCodensity

runInstr :: InstructionF Robot -> Robot -> Robot
runInstr Stop x = x
runInstr (Pure x) _ = x
runInstr (TurnRight f) x = runInstr (f y) y
  where
    y = x & direction %~ _turnRight
runInstr (TurnLeft f) x = runInstr (f y) y
  where
    y = x & direction %~ _turnLeft
runInstr (Advance f) x = runInstr (f y) y
  where
    y = _advance x

runRobot :: Instruction -> Robot -> Robot
runRobot = runInstr . lowerCodensity

bearing :: Robot -> Bearing
bearing = _direction

coordinates :: Robot -> (Integer, Integer)
coordinates = _coord

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot _direction _coord = Robot {..}

move :: Robot -> String -> Robot
move = flip (runRobot . foldMap toInstruction)
