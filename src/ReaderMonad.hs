module ReaderMonad (Modular, unModular) where

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Ratio (denominator, numerator)

import Reciprocal (modularReciprocal)

newtype Modular a
  = Modular (Reader a a)

instance (Integral a) => Num (Modular a) where
  fromInteger = toModular . pure . fromInteger
  Modular m1 + Modular m2 = toModular $ liftA2 (+) m1 m2
  Modular m1 * Modular m2 = toModular $ liftA2 (*) m1 m2
  negate (Modular m)      = toModular $ negate <$> m
  abs (Modular m)         = toModular $ abs <$> m
  signum (Modular m)      = toModular $ signum <$> m

instance (Integral a) => Fractional (Modular a) where
  recip (Modular m) = toModular $ liftA2 modularReciprocal ask m
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

unModular :: a -> Modular a -> a
unModular modulus (Modular m) = runReader m modulus

toModular :: (Integral a) => Reader a a -> Modular a
toModular m = Modular $ liftA2 mod m ask
