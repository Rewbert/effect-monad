{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, TypeFamilies,
             MultiParamTypeClasses, FlexibleInstances, UndecidableInstances,
             ScopedTypeVariables, PolyKinds, FlexibleContexts #-}

module Control.Category.Writer(Writer(..), Symbol, put, Mapping(..),
                             IsMap, Map(..), union, Var(..),
                             Union, Unionable) where

import Control.CMonad
import Data.Type.Map
import Data.Monoid
import GHC.TypeLits
import Prelude hiding (Monad(..))
import GHC.Exts ( Constraint )    

{-| Provides an effect-parameterised version of the writer monad. Effects
   are maps of variable-type pairs, providing an effect system for writer effects. -}

data Writer (pre  :: [Mapping Symbol NatNum])
            (post :: [Mapping Symbol NatNum])
            (w    :: [Mapping Symbol *])
             a =
        Writer { runWriter :: (a, Map w) }

instance CMonad Writer where
    type CInv Writer s t = (IsMap s, IsMap t, Unionable s t)

    {-| A trivial effect is the empty map -}
    type Identity Writer = '[]
    {-| Effects are combined by map union -}
    type Comp Writer s t = Union s t

    {-| Trivially pure computation produces an empty state -}
    return x = Writer (x, Empty)
    {-| Composing copmutations takes the union of the writer states, using the monoid
        operation to combine writes to the same variable -}
    (Writer (a, w)) >>= k = let Writer (b, w') = k a
                            in  Writer (b, w `union` w')

{-| Write to variable 'v' with value of type 'a' -}
put :: Var v -> a -> Writer pre (Inc v pre) '[v :-> a] ()
put v a = Writer ((), Ext v a Empty)

type family Inc (k :: a) (m :: [Mapping a v]) :: [Mapping a v] where
        Inc k ((k :-> v) ': xs) = (k :-> (Suc v)) ': xs
        Inc k (x ': xs)         = x ': (Inc k xs)

-- Values of the same type can be combined
type instance Combine v v = v

{-| Define the operation for removing duplicates using mappend -}
instance (Monoid u, Nubable ((k :-> u) ': s)) => Nubable ((k :-> u) ': (k :-> u) ': s) where
    nub (Ext _ u (Ext k v s)) = nub (Ext k (u `mappend` v) s)

{- Sub effecting for the parametric effect monad -}
instance Supermap s t => Subeffect Writer s t where
    sub (Writer (a, w)) = Writer (a, (supermap w)::(Map t))

{-| Computes supermaps of maps of variable-type mappings, using the 'mempty' operation  -}
class Supermap s t where
    supermap :: Map s -> Map t

instance Supermap '[] '[] where
    supermap Empty = Empty

instance (Monoid x, Supermap '[] s) => Supermap '[] ((k :-> x) ': s) where
    supermap Empty = Ext Var mempty (supermap Empty)

instance Supermap s t => Supermap ((k :-> v) ': s) ((k :-> v) ': t) where
    supermap (Ext k x xs) = Ext k x (supermap xs)

instance Monoid Int where
        mempty = 0
        mappend x y = x + y

data NatNum where
        Z   :: NatNum
        Suc :: NatNum -> NatNum
        
type family (n :: NatNum) :+ (m :: NatNum) where
        n :+ Z     = n
        n :+ (Suc m) = Suc (n :+ m)

varX = Var :: (Var "x")
varY = Var :: (Var "y")

-- | example

prop :: Writer '["x" :-> n, "y" :-> m]
               '["x" :-> (Suc (Suc (Suc n))), "y" :-> (Suc (Suc m))]
               '["x" :-> Int, "y" :-> String]
                ()
prop =  put varX (42 :: Int) >>
        put varY "hello"     >>
        put varX (58 :: Int) >>
        put varX (58 :: Int) >>
        put varY " world"


propSingle :: Writer '["x" :-> n]
               '["x" :-> (Suc (Suc n))]
               '["x" :-> Int]
                ()
propSingle =  put varX (42 :: Int) >>
              put varX (58 :: Int) 
