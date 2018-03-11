{-# LANGUAGE KindSignatures, DataKinds, TypeOperators, GADTs, TypeFamilies, ConstraintKinds, PolyKinds, MultiParamTypeClasses, InstanceSigs #-}

{-| Module containing a CMonad wrapper for Effect Monads.-}

module Control.Category.Effect where

import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )
import Control.CMonad
import qualified Control.Effect as E

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
    (Wrap x) >>= f = Wrap $ x E.>>= (unWrap . f)
