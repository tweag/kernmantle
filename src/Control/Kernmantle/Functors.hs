{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

-- | This module provides Functors/Profunctors over binary effects

module Control.Kernmantle.Functors where

import Data.Profunctor
import Data.Bifunctor.Tannen


-- | Functors over binary effects. Same as the ProfunctorFunctor class from
-- @profunctors@ but with less requirements over mapped effs.
class EffFunctor f where
  effmap :: (eff :-> eff') -> f eff :-> f eff'

-- | Pointed Functors (= functors equipped with 'pure') over binary
-- effects. Doesn't have an equivalent afaik in @profunctors@.
class (EffFunctor f) => EffPointedFunctor f where
  effpure :: eff :-> f eff

-- | Would be a "ProfunctorProfunctor", but that class doesn't exist.
class EffProfunctor p where
  effdimap :: (a' :-> a) -> (b :-> b') -> p a b :-> p a' b'



-- | Adds some ahead-of-time (functorial) unary effects to any binary effect,
-- that should be executed /before/ we get to the binary effect. These
-- ahead-of-time effects can be CLI parsing, access to some configuration,
-- pre-processing of the compute graph, etc.
type WithAoT a f = Tannen f a

withAoT :: f (eff x y) -> (eff `WithAoT` f) x y
withAoT = Tannen

instance (Functor f) => EffFunctor (Tannen f) where
  effmap f = Tannen . fmap f . runTannen
  {-# INLINE effmap #-}

instance (Applicative f) => EffPointedFunctor (Tannen f) where
  effpure = Tannen . pure
  {-# INLINE effpure #-}
