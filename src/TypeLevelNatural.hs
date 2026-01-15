{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
module TypeLevelNatural (Modular(unModular)) where

import           Data.Ratio   (denominator, numerator)
import           GHC.TypeLits (KnownNat, Nat, natVal)

import           Reciprocal   (modularReciprocal)

newtype Modular a (modulus :: Nat) =
  Modular { unModular :: a }
  deriving (Eq)

instance Show a => Show (Modular a modulus) where
  show (Modular n) = show n

instance (Integral a, KnownNat modulus) => Num (Modular a modulus) where
  fromInteger           = toModular . fromInteger
  Modular n + Modular m = toModular $ n + m
  Modular n * Modular m = toModular $ n * m
  negate (Modular n)    = toModular $ negate n
  abs (Modular n)       = toModular $ abs n
  signum (Modular n)    = toModular $ signum n

instance (Integral a, KnownNat modulus) => Fractional (Modular a modulus) where
  recip m@(Modular n) = toModular $ modularReciprocal (fromInteger (natVal m)) n
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

toModular :: (Integral a, KnownNat modulus) => a -> Modular a modulus
toModular @_ @modulus n = Modular $ n `mod` fromInteger (natVal @modulus undefined)
