{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}

-- | This module provides the API for effects ('EffBuilder') that will construct
-- other effects

module Control.Kernmantle.Builder where

import qualified Control.Applicative as App
import Control.Arrow hiding (first, second)
import Control.Category
import Data.Biapplicative (Biapplicative)
import Data.Bifunctor
import Data.Bifunctor.Functor
import Data.Bifunctor.Sum
import Data.Bifunctor.Tannen
import Data.Profunctor hiding ((:->))
import Data.Profunctor.Cayley
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

instance (Applicative f) => EffPointedFunctor (Tannen f) where
  effpure = Tannen . pure
  {-# INLINE effpure #-}

-- | Would be a "@BifunctorBifunctor@", but that class doesn't exist
class (forall a. (Arrow a) => (EffFunctor (p a))) => EffBifunctor p where
  effbimap :: (Arrow a) => (a :-> a') -> (b :-> b') -> p a b :-> p a' b'
  effbimap f g = efffirst f . effsecond g

  efffirst :: (Arrow a) => (a :-> a') -> p a b :-> p a' b
  efffirst f = effbimap f id

-- | Would be a "@ProfunctorBifunctor@", but that class doesn't exist.
class (forall a. (EffFunctor (p a))) => EffProfunctor p where
  effdimap :: (a' :-> a) -> (b :-> b') -> p a b :-> p a' b'
  effdimap f g = efflmap f . effrmap g

  efflmap :: (a' :-> a) -> p a b :-> p a' b
  efflmap f = effdimap f id

-- | A binary effect @builder@ that returns (builds) another binary effect
-- @runner@, based on some input type @i@.
--
-- Use 'effpure' to just wrap a @runner a b@ effect inside the 'EffBuilder', and
-- 'effbuild' to create a @builder@ effect that will return the @runner@
-- effect.
--
-- Use 'efffirst' to change the @builder@ effect and 'efffsecond' to change the
-- @runner@ effect (or 'effbimap' to change both altogether).
newtype EffBuilder i builder runner a b =
  EffBuilder { runEffBuilder :: builder i (runner a b) }

  deriving ( Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , ThrowEffect ex, TryEffect ex
           , Profunctor, Strong, Choice, Costrong )
    via (Cayley (App.WrappedArrow builder i) runner)

  deriving ( EffFunctor, EffPointedFunctor )
    via (Tannen (App.WrappedArrow builder i))

  deriving ( Bifunctor, Biapplicative )
    via (Tannen (App.WrappedArrow builder i) runner)

instance EffBifunctor (EffBuilder i) where
  efffirst f (EffBuilder eb) = EffBuilder $ f eb

-- | Wraps an effect returning another effect inside an 'EffBuilder'
effbuild :: builder i (runner a b) -> EffBuilder i builder runner a b
effbuild = EffBuilder

-- | An 'EffBuilder' that requires no input
type UnaryBuilder f = EffBuilder () (Kleisli f)

-- | Turns a unary effect returning an effect into an 'EffBuilder'
unaryBuilder :: f (eff a b) -> UnaryBuilder f eff a b
unaryBuilder f = EffBuilder (Kleisli $ const f)

-- | Unwraps a unary effect from a 'SimpleEffBuilder'
unaryFromBuilder :: UnaryBuilder f eff a b -> f (eff a b)
unaryFromBuilder (EffBuilder (Kleisli f)) = f ()

-- | Wraps an effect so that it can be bypassed
type Bypass = Sum (->)

-- | Don't bypass an effect
performEff :: eff a b -> Bypass eff a b
performEff = R2

-- | Bypass an effect, using some pure function instead
bypassEff :: (a -> b) -> Bypass eff a b
bypassEff = L2

-- | Bypass an effect, always returning some pure value instead
bypassEff_ :: b -> Bypass eff a b
bypassEff_ = L2 . const

-- | Runs a @Bypass eff@ in some @Arrow@
runBypassWith :: (Arrow arr) => (eff :-> arr) -> Bypass eff a b -> arr a b
runBypassWith _ (L2 f) = arr f
runBypassWith f (R2 eff) = f eff
