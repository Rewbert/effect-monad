{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

{-| Provides a way to 'count' in the type-level with a monadic interface
    to sum up the individual counts of subcomputations -}

module Control.Category.Counter where

import Control.CMonad
import Prelude hiding (Monad(..))

-- | Type constructors for natural numbers.
data Nat where
        Z   :: Nat
        Suc :: Nat -> Nat

-- | A data type wich describe the number of performed computation in its types
newtype Counter (pre :: *) (post :: *) (n :: Nat) a = Count { forget :: a }

-- | Type level addition of natural numbers
type family (n :: Nat) :+ (m :: Nat) where
        n :+ Z     = n
        n :+ Suc m = Suc (n :+ m)

instance Show a => Show (Counter i j k a) where
        show (Count a) = "Count " ++ show a

instance CMonad Counter where
  type CInv Counter pre int post f g = (pre ~ (), int ~ (), post ~ ())
  type Identity Counter = Z
  type EmptyCond Counter = ()
  type Comp Counter n m = n :+ m

  -- On the value level we just 'work' with the a
  return = Count

  -- On the value level we just 'work' with the a
  (Count a) >>= k = Count . forget $ k a

-- | Short hand for a 'Counter' that has perfomed one 'Int' computation
one   :: Counter () () (Suc Z)             Int
one = Count 1
-- | Short hand for a 'Counter' that has perfomed two 'Int' computations
two   :: Counter () () (Suc (Suc Z))       Int
two = Count 2
-- | Short hand for a 'Counter' that has perfomed three 'Int' computations
three :: Counter () () (Suc (Suc (Suc Z))) Int
three = Count 3

-- | Perform one computation, and increments the counter
tick :: a -> Counter () () (Suc Z) a
tick = Count
