{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses, InstanceSigs #-}

module Control.CMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )
import qualified Control.Effect as E

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

-- | Automatic converion of Effect-monads into CMonads
data Effect m pre post k a where
  Wrap :: (E.Effect m)
       => m (e :: k) a -> Effect m () () (e :: k) a

{- | Run function for the Effect CMonads, results in the respective effect
     computation -}
unWrap :: (E.Effect m)
       => Effect m () () e a -> m e a
unWrap (Wrap m) = m

instance E.Effect m => CMonad (Effect m)
  where
    type CInv (Effect m) pre int post f g =
      (pre ~ (), int ~ (), post ~ (), E.Inv m f g)
    type Identity (Effect m) = E.Unit m
    type EmptyCond (Effect m) = ()
    type Comp (Effect m) s t = E.Plus m s t
    return x = Wrap (E.return x)
    (Wrap x) >>= f = Wrap $ x E.>>= \y -> unWrap (f y)
