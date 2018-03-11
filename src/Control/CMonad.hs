{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ConstraintKinds, PolyKinds,
             MultiParamTypeClasses #-}

{- | The CMonad package! -}
module Control.CMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )

{-| Specified our monad, which is a parameterised annotated effect monad.
    It is built upon the basic monad but with type-level invariants and
    annotated by a type-level monoid. -}
class CMonad (m :: l -> l -> k -> * -> *) where

  -- | Effect of a trivially effectful computation
  type Identity m :: k

  -- | The pre and post conditions for a return
  type EmptyCond m :: l

  -- | Composing the effects of two subcomputations
  type Comp m (f :: k) (g :: k) :: k

  -- | Allows for constraints on '>>='
  type CInv m (pre :: l) (int :: l) (post :: l) (f :: k) (g :: k) :: Constraint
  type CInv m pre int post f g = ()

  {- | CMonad version of 'return'. Require 'EmptyCond' as both pre-condition
       and post-condition. Annotated by the 'Identity' effect. -}
  return :: a -> m (EmptyCond m) (EmptyCond m) (Identity m) a

  {-| CMonad version of '>>=' (bind). The invariant of the resulting
      computation will have the same pre condition as the first subcomputation
      and the post condition of the last. The post condition of the first
      computation must match the pre condition of the second. The resulting
      effect will be annotated by 'Comp'. -}
  (>>=) :: (CInv m pre int post f g)
        => m pre int f a
        -> (a -> m int post g b)
        -> m pre post (Comp m f g) b

  -- | CMonad version of '>>'.
  (>>)  :: (CInv m pre int post f g)
        => m pre int  f a
        -> m int post g b
        -> m pre post (Comp m f g) b
  m >> g = m >>= const g

-- | Specifies subeffecting behaviour
class Subeffect (m :: l -> l -> k -> * -> *) f g where
  sub :: m pre post f a -> m pre post g a
