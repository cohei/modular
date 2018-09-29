{-# LANGUAGE FlexibleContexts #-}
module Reflection (Modular(unModular)) where

import           Data.Ratio      (denominator, numerator)
import           Data.Reflection (Given (given))

import           Reciprocal      (modularReciprocal)

newtype Modular a =
  Modular { unModular :: a }
  deriving (Eq)

instance Show a => Show (Modular a) where
  show (Modular n) = show n

instance (Given a, Integral a) => Num (Modular a) where
  fromInteger           = toModular . fromInteger
  Modular n + Modular m = toModular $ n + m
  Modular n * Modular m = toModular $ n * m
  negate (Modular n)    = toModular $ negate n
  abs (Modular n)       = toModular $ abs n
  signum (Modular n)    = toModular $ signum n

instance (Given a, Integral a) => Fractional (Modular a) where
  recip (Modular n) = toModular $ modularReciprocal given n
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

toModular :: (Given a, Integral a) => a -> Modular a
toModular = Modular . (`mod` given)
