{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, EmptyDataDecls, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.CMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )    

class CMonad (m :: l -> l -> k -> * -> *) where
  type Identity m :: k
  type Comp m (f :: k) (g :: k) :: k
  type CInv m (f :: k) (g :: k) :: Constraint
  type CInv m f g = ()
  return :: a -> m inv inv (Identity m) a
  (>>=) :: CInv m f g
        => m pre int f a 
        -> (a -> m int post g b)
        -> m pre post (Comp m f g) b
  (>>)  :: CInv m f g
        => m pre int  f a
        -> m int post g b
        -> m pre post (Comp m f g) b
  m >> g = m >>= (\_ -> g)

class Subeffect (m :: l -> l -> k -> * -> *) f g where
        sub :: m pre post f a -> m pre post g a
