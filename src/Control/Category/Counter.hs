{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, EmptyDataDecls, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.Category.Counter where

import Control.CMonad
import Prelude hiding (Monad(..))

data Nat where
        Z   :: Nat
        Suc :: Nat -> Nat

data Counter (pre :: l) (post :: l) (n :: Nat) a = Count { forget :: a }

type family (n :: Nat) :+ (m :: Nat) where
        n :+ Z     = n
        n :+ (Suc m) = Suc (n :+ m)

instance Show a => Show (Counter i j k a) where
        show (Count a) = "Count " ++ show a

instance CMonad Counter where
    type Identity Counter = Z
    type Comp Counter n m = n :+ m

    -- On the value level we just 'work' with the a
    return a = Count a

    -- On the value level we just 'work' with the a
    (Count a) >>= k = Count . forget $ k a

one   :: Counter () () (Suc Z)             Int
one = Count 1
two   :: Counter () () (Suc (Suc Z))       Int
two = Count 2
three :: Counter () () (Suc (Suc (Suc Z))) Int
three = Count 3

tick :: Int -> Counter () () (Suc Z) Int
tick a = Count a
