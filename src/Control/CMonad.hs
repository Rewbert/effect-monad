{-# LANGUAGE KindSignatures, TypeOperators, EmptyDataDecls, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses #-}

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

data Z
data S n

{-| The counter has no semantic meaning -}
data Counter pre post n a = Count { forget :: a }

{-| Type-level addition -}
type family n :+ m 
type instance n :+ Z     = n
type instance n :+ (S m) = S (n :+ m)

instance CMonad Counter where
    type CInv Counter n m = ()
    type Identity Counter = Z
    type Comp Counter n m = n :+ m

    -- On the value level we just 'work' with the a
    return a = Count a

    -- On the value level we just 'work' with the a
    (Count a) >>= k = Count . forget $ k a
