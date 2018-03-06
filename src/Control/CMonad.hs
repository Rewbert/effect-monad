{-# LANGUAGE KindSignatures, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.CMonad where

import Control.Effect
import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )    

class CMonad (m :: l -> l -> k -> * -> *) where
  type Identity m :: k
  type Comp m (f :: k) (g :: k) :: k
  type CInv m (f :: k) (g :: k) :: Constraint
  type CInv m f g = ()
  return :: a -> m inv inv (Identity m) a
  (>>=) :: CInv m f g => m pre int f a -> (a -> m int post g b) -> m pre post (Comp m f g) b
