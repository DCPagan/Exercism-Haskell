{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
module WordProblem where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Fix
import Data.Bifunctor.TH
import Data.Monoid
import Text.ParserCombinators.ReadP (ReadP, char, eof, string)
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read (readMaybe, readPrec)
import Text.Read.Lex (readDecP)

{-
 - Algebraic field structure
 -}
data FieldF a b where
  Pure :: a -> FieldF a b
  AddUnit :: FieldF a b
  MultUnit :: FieldF a b
  AddInverse :: b -> FieldF a b
  MultInverse :: b -> FieldF a b
  Plus :: b -> b -> FieldF a b
  Multiply :: b -> b -> FieldF a b
  deriving (Eq, Functor)
deriveBifunctor ''FieldF

instance (Show a, Show b) => Show (FieldF a b) where
  show = \case
    Pure x -> show x
    AddUnit -> "0"
    MultUnit -> "1"
    AddInverse x -> "-" ++ show x
    MultInverse x -> "reciprocal (" ++ show x ++ ")"
    Plus x y -> show x ++ " plus " ++ show y
    Multiply x y -> show x ++ " multiplied by " ++ show y

type Field a = Fix (FieldF a)

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

add :: Field a -> Field a -> Field a
add a (Fix AddUnit) = a
add (Fix AddUnit) a = a
add a b = Fix $ Plus a b

multiply :: Field a -> Field a -> Field a
multiply a (Fix MultUnit) = a
multiply (Fix MultUnit) a = a
multiply a b = Fix $ Multiply a b

readNumber :: (Eq a, Num a) => ReadP (Field a)
readNumber = do
  s <- AddInverse . Fix <$ (char '-') <|> pure id
  x <- readDecP
  return $ Fix $ case x of
    0 -> AddUnit
    1 -> s MultUnit
    _ -> s $ Pure x

readProduct :: (Eq a, Num a) => ReadP (Field a)
readProduct = do
  x <- readNumber
  ys <- many $ do
    op <- id <$ string " multiplied by "
      <|> (Fix . MultInverse) <$ string " divided by "
    op <$> readNumber
  return $ multiply x $ foldr multiply (Fix MultUnit) ys

readSum :: (Eq a, Num a) => ReadP (Field a)
readSum = do
  x <- readProduct
  ys <- many $ do
    op <- id <$ string " plus "
      <|> (Fix . AddInverse) <$ string " minus "
    op <$> readProduct
  return $ add x $ foldr add (Fix AddUnit) ys

readExpressionIgnoringPrecedence :: (Eq a, Num a) => ReadP (Field a)
readExpressionIgnoringPrecedence = do
  x <- readNumber
  ys <- many $ do
    (op, inv) <- (add, id) <$ string " plus "
      <|> (add, Fix . AddInverse) <$ string " minus "
      <|> (multiply, id) <$ string " multiplied by "
      <|> (multiply, Fix . MultInverse) <$ string " divided by "
    y <- readNumber
    return $ flip op $ inv y
  return $ (appEndo . getDual $ foldMap (Dual . Endo) ys) x

readField :: (Eq a, Num a) => ReadP (Field a)
readField = string "What is " *> readSum <* char '?' <* eof

readFieldIgnoringPrecedence :: (Eq a, Num a) => ReadP (Field a)
readFieldIgnoringPrecedence
  = string "What is " *> readExpressionIgnoringPrecedence <* char '?' <* eof

instance {-# OVERLAPPING #-} (Eq a, Num a) => Read (Field a) where
  readPrec = lift readFieldIgnoringPrecedence

instance {-# OVERLAPPING #-} Show a => Show (Field a) where
  show (Fix e) = case e of
    Pure x -> show x
    AddUnit -> "0"
    MultUnit -> "1"
    Plus x y -> case unFix y of
      AddUnit -> show x
      AddInverse z -> show x ++ " minus " ++ show z
      _ -> show x ++ " plus " ++ show y
    Multiply x y -> case unFix y of
      MultUnit -> show x
      MultInverse z -> show x ++ " divided by " ++ show z
      _ -> show x ++ " multiplied by " ++ show y
    AddInverse x -> "-" ++ show x
    MultInverse x -> "1/" ++ show x

evalRationalField :: (Eq a, Fractional a) => FieldF a (Maybe a) -> Maybe a
evalRationalField = \case
  Pure x -> Just x
  AddUnit -> Just 0
  MultUnit -> Just 1
  AddInverse x -> negate <$> x
  MultInverse (Just 0) -> Nothing
  MultInverse x -> recip <$> x
  Plus x y -> liftA2 (+) x y
  Multiply x y -> liftA2 (*) x y

answer :: String -> Maybe Integer
answer = readMaybe @(Field Rational) >=> foldFix evalRationalField
  >>> fmap floor
