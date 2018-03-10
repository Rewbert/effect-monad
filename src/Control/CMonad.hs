{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses, InstanceSigs #-}

module Control.CMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )

{-| Specified our monad, which is a parameterised annotated effect monad.
    It is built upon the basic monad but with type-level invariants and
    annotated by a type-level monoid. -}
class CMonad (m :: l -> l -> k -> * -> *) where

  {-| Effect of a trivially effectful computation -}
  type Identity m :: k
  type EmptyCond m :: l
  type Comp m (f :: k) (g :: k) :: k
  type CInv m (pre :: l) (int :: l) (post :: l) (f :: k) (g :: k) :: Constraint
  type CInv m pre int post f g = ()
  return :: a -> m (EmptyCond m) (EmptyCond m) (Identity m) a
  (>>=) :: (CInv m pre int post f g)
        => m pre int f a
        -> (a -> m int post g b)
        -> m pre post (Comp m f g) b
  (>>)  :: (CInv m pre int post f g)
        => m pre int  f a
        -> m int post g b
        -> m pre post (Comp m f g) b
  m >> g = m >>= const g

-- | Specifies subeffecting behaviour
class Subeffect (m :: l -> l -> k -> * -> *) f g where
  sub :: m pre post f a -> m pre post g a
