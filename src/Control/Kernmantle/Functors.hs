{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}

-- | This module provides Functors/Profunctors over binary effects

module Control.Kernmantle.Functors where

import qualified Control.Applicative as App
import Control.Arrow
import Control.Category
import Data.Biapplicative (Biapplicative)
import Data.Bifunctor (Bifunctor)
import Data.Bifunctor.Functor
import Data.Bifunctor.Tannen
import Data.Profunctor hiding ((:->))
import qualified Data.Profunctor as Pro

import Control.Kernmantle.Error

import Prelude hiding (id, (.))


-- | Functors over binary effects
type EffFunctor = BifunctorFunctor

-- | Maps the effect inside an 'EffFunctor'
effrmap :: (EffFunctor f) => (eff :-> eff') -> f eff :-> f eff'
effrmap = bifmap

-- | Pointed Functors (= functors equipped with 'pure') over binary
-- effects. Doesn't have an equivalent afaik in @bifunctors@.
class (EffFunctor f) => EffPointedFunctor f where
  effpure :: eff :-> f eff

-- | Would be a "ProfunctorBifunctor", but that class doesn't exist.
class (forall a. EffFunctor (p a)) => EffProfunctor p where
  effdimap :: (a' :-> a) -> (b :-> b') -> p a b :-> p a' b'
  effdimap f g = efflmap f . effrmap g

  efflmap :: (a' :-> a) -> p a b :-> p a' b
  efflmap f = effdimap f id


-- | Adds some ahead-of-time unary (functorial) effects to any binary effect,
-- that should be executed /before/ we get to the binary effect. These
-- ahead-of-time effects can be CLI parsing, access to some configuration,
-- pre-processing of the compute graph, etc.
type WrappingEff = Tannen

-- | Adds the 'WrappingEff' layer
withEffWrapper :: f (eff x y) -> (f `WrappingEff` eff) x y
withEffWrapper = Tannen

-- | Removes the 'WrappingEff' layer
getEffWrapper :: (f `WrappingEff` eff) x y -> f (eff x y)
getEffWrapper = runTannen

instance (Applicative f) => EffPointedFunctor (WrappingEff f) where
  effpure = withEffWrapper . pure
  {-# INLINE effpure #-}


-- | A binary effect that returns (builds) another binary effect, based on some
-- input type @i@
newtype EffBuilder i f g a b = EB { runEffBuilder :: f i (g a b) }

  deriving ( Bifunctor, Biapplicative
           , Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , ThrowEffect ex, TryEffect ex
           )
    via (Tannen (App.WrappedArrow f i) g)

  deriving (EffFunctor, EffPointedFunctor)
    via (Tannen (App.WrappedArrow f i))

  deriving (Profunctor, Strong, Choice, Costrong)
    via (Pro.WrappedArrow (Tannen (App.WrappedArrow f i) g))

-- instance EffProfunctor (EffBuilder i) where
--   effdimap f g =
