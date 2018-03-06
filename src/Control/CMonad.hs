{-# LANGUAGE KindSignatures, TypeOperators, EmptyDataDecls, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

module Control.CMonad where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )    

class CMonad (m :: l -> l -> k -> * -> *) where
  type Identity m :: k
  type Comp m (a :: l) (f :: k) (g :: k) :: k
  type CInv m (f :: k) (g :: k) :: Constraint
  type CInv m (a :: l) f g = ()
  return :: a -> m inv inv (Identity m) a
  (>>=) :: CInv m f g
        => m pre int f a 
        -> (a -> m int post g b)
        -> m pre post (Comp m post f g) b

data Z
data S n

{-| The counter has no semantic meaning -}
data Counter i j n a = Counter { forget :: a }

{-| Type-level addition -}
type family n :+ m 
type instance n :+ Z     = n
type instance n :+ (S m) = S (n :+ m)

instance CMonad Counter where
        type Identity Counter = Z
        type Comp Counter a f g = f :+ g
        type CInv Counter a n m = ()
