{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DerivingVia #-}

-- | This module provides some missing classes from profunctors
--
-- BEWARE: That part of Kernmantle API is experimental and is likely to change
-- in the future.

module Data.Profunctor.EffFunctor where

import qualified Control.Applicative as App
import Control.Arrow hiding (first, second)
import Control.Category
import Data.Biapplicative (Biapplicative)
import Data.Bifunctor
import Data.Bifunctor.Functor
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
