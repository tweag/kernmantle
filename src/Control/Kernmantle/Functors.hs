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
import Control.Arrow hiding (first, second)
import Control.Category
import Data.Biapplicative (Biapplicative)
import Data.Bifunctor
import Data.Bifunctor.Functor
import Data.Bifunctor.Tannen
import Data.Profunctor hiding ((:->))
import qualified Data.Profunctor as Pro

import Control.Kernmantle.Error

import Prelude hiding (id, (.))


-- | Functors over binary effects
type EffFunctor = BifunctorFunctor

-- | Maps the effect inside an 'EffFunctor'. Various names to follow the
-- @bifunctors@/@profunctors@ conventions.
effmap, effrmap, effsecond :: (EffFunctor f) => (eff :-> eff') -> f eff :-> f eff'
effmap = bifmap
effrmap = bifmap
effsecond = bifmap

-- | Pointed Functors (= functors equipped with 'pure') over binary
-- effects. Doesn't have an equivalent afaik in @bifunctors@.
class (EffFunctor f) => EffPointedFunctor f where
  effpure :: eff :-> f eff

-- | Would be a "BifunctorBifunctor", but that class doesn't exist
class (forall a. (Arrow a) => (EffFunctor (p a))) => EffBifunctor p where
  effbimap :: (Arrow a) => (a :-> a') -> (b :-> b') -> p a b :-> p a' b'
  effbimap f g = efffirst f . effsecond g

  efffirst :: (Arrow a) => (a :-> a') -> p a b :-> p a' b
  efffirst f = effbimap f id

-- | Would be a "ProfunctorBifunctor", but that class doesn't exist.
class (forall a. (EffFunctor (p a))) => EffProfunctor p where
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


-- | A binary effect @builder@ that returns (builds) another binary effect
-- @runner@, based on some input type @i@. Use 'efffirst' to change the
-- @builder@ effect and 'efffsecond' to change the @runner@ effect (or
-- 'effbimap' to change both altogether).
newtype EffBuilder i builder runner a b =
  EffBuilder { runEffBuilder :: builder i (runner a b) }

  deriving ( Bifunctor, Biapplicative
           , Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , ThrowEffect ex, TryEffect ex
           )
    via (Tannen (App.WrappedArrow builder i) runner)

  deriving (EffFunctor, EffPointedFunctor)
    via (Tannen (App.WrappedArrow builder i))

  deriving (Profunctor, Strong, Choice, Costrong)
    via (Pro.WrappedArrow (Tannen (App.WrappedArrow builder i) runner))

instance EffBifunctor (EffBuilder i) where
  efffirst f (EffBuilder eb) = EffBuilder $ f eb
