{-# LANGUAGE DataKinds, TypeOperators, GADTs, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses, InstanceSigs #-}

{-| Module containing a CMonad wrapper for Monads.-}

module Control.Category.Monad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )
import Control.CMonad
import qualified Prelude as M (Monad(..))

-- | Wrapepr for regular Monads
data Monad m pre post k a where
  Wrap :: (M.Monad m) => m a -> Monad m () () () a

-- | Unwraps a monad
unWrap :: (M.Monad m) => Monad m pre post k a -> m a
unWrap (Wrap m) = m

instance (M.Monad m) => CMonad (Monad m) where
  type CInv (Monad m) pre int post f g =
    (pre ~ (), int ~ (), post ~ (), f ~ (), g ~ ())
  type Identity (Monad m)  = ()
  type EmptyCond (Monad m) = ()
  type Comp (Monad m) s t  = ()
  return x = Wrap (M.return x)
  (Wrap x) >>= f = Wrap $ x M.>>= (unWrap . f)
