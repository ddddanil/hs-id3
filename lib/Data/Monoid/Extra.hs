module Data.Monoid.Extra where

import Data.Bits
import Control.Lens.Iso
import Control.Lens.Wrapped
import GHC.Generics

newtype Disjunction a = Disjunction { getDisjunction :: a }
  deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num, Functor)
  deriving newtype (Bits, Real, Enum, Integral)

instance Bits a => Semigroup (Disjunction a) where
  (<>) = (.|.)

instance Bits a => Monoid (Disjunction a) where
  mempty = zeroBits

instance Applicative (Disjunction) where
  pure = Disjunction
  (<*>) = coerce

instance Monad Disjunction where
  m >>= k = k (getDisjunction m)

instance Wrapped (Disjunction a) where
  type Unwrapped (Disjunction a) = a
  _Wrapped' = iso getDisjunction Disjunction


newtype Conjunction a = Conjunction { getConjunction :: a }
  deriving (Eq , Ord, Read, Show, Bounded, Generic, Generic1, Num, Functor)
  deriving newtype (Bits, Real, Enum, Integral)

instance Bits a => Semigroup (Conjunction a) where
  (<>) = (.&.)

instance Bits a => Monoid (Conjunction a) where
  mempty = complement zeroBits

instance Applicative (Conjunction) where
  pure = Conjunction
  (<*>) = coerce

instance Monad Conjunction where
  m >>= k = k (getConjunction m)

instance Wrapped (Conjunction a) where
  type Unwrapped (Conjunction a) = a
  _Wrapped' = iso getConjunction Conjunction
