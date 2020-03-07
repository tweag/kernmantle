{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}

-- | This module exposes the SieveTrans class and some Sieve transformers based
-- on usual Reader and Writer

module Data.Profunctor.Trans where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Bifunctor.Tannen
import Data.Profunctor
import Data.Profunctor.Cayley

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer.Strict as W


-- | A general version of 'Sieve' that allows mapping and recursively reaching
-- the sieve
class SieveTrans f cat | cat -> f where
  liftSieve :: (a -> f b) -> cat a b
  mapSieve :: ((a -> f b) -> (a' -> f b')) -> cat a b -> cat a' b'

-- | Just an alias
type HasKleisli = SieveTrans

-- | Just an alias
liftKleisli :: (HasKleisli m eff) => (a -> m b) -> eff a b
liftKleisli = liftSieve
{-# INLINE liftKleisli #-}

-- | Just an alias
mapKleisli :: (HasKleisli m eff)
           => ((a -> m b) -> (a' -> m b')) -> eff a b -> eff a' b'
mapKleisli = mapSieve
{-# INLINE mapKleisli #-}

instance SieveTrans f (Star f) where
  liftSieve = Star
  mapSieve f (Star m) = Star $ f m

instance SieveTrans m (Kleisli m) where
  liftSieve = Kleisli
  mapSieve f (Kleisli m) = Kleisli $ f m

instance (SieveTrans f cat, Applicative f')
  => SieveTrans f (Cayley f' cat) where
  liftSieve = Cayley . pure . liftSieve
  {-# INLINE liftSieve #-}
  mapSieve f (Cayley app) = Cayley $ mapSieve f <$> app
  {-# INLINE mapSieve #-}

instance (SieveTrans f cat, Applicative f)
  => SieveTrans f (Tannen f cat) where
  liftSieve = Tannen . pure . liftSieve
  {-# INLINE liftSieve #-}
  mapSieve f (Tannen app) = Tannen $ mapSieve f <$> app
  {-# INLINE mapSieve #-}

type HasKleisliIO m eff = (HasKleisli m eff, MonadIO m)

-- | When you want to lift some IO action in a Sieve of any MonadIO
liftKleisliIO :: (HasKleisliIO m eff) => (a -> IO b) -> eff a b
liftKleisliIO f = liftKleisli $ liftIO . f
{-# INLINE liftKleisliIO #-}

-- | An alias to make signatures more readable
type (~>) = Cayley
infixr 1 ~>  -- To be of a lower precedence than (:->)

type Reader r = R.Reader r
type Writer w = W.Writer w

fmapping :: (Functor f) => f t -> (t -> eff a b) -> (f ~> eff) a b
fmapping a f = Cayley $ fmap f a
{-# INLINE fmapping #-}

-- | mapCayley in profunctors maps the functor. mapCayleyEff maps the effect in
-- it.
mapCayleyEff :: (Functor f)
             => (eff a b -> eff' a' b')
             -> (f ~> eff) a b
             -> (f ~> eff') a' b'
mapCayleyEff f (Cayley eff) = Cayley $ fmap f eff
{-# INLINE mapCayleyEff #-}

reading :: (t -> eff a b) -> (Reader t ~> eff) a b
reading f = Cayley $ R.reader $ f
{-# INLINE reading #-}

mapReader_ :: (t -> eff a b -> eff' a' b')
           -> (Reader t ~> eff) a b
           -> (Reader t ~> eff') a' b'
mapReader_ f (Cayley eff) = Cayley (R.reader $ \x -> f x $ R.runReader eff x)
{-# INLINE mapReader_ #-}

mapReader :: (t' -> eff a b -> (t, eff' a' b'))
          -> (Reader t ~> eff) a b
          -> (Reader t' ~> eff') a' b'
mapReader f (Cayley eff) = Cayley (R.reader $ \t' ->
  let (t,eff') = f t' $ R.runReader eff t
  in eff')
{-# INLINE mapReader #-}

runReader :: t -> (Reader t ~> eff) a b -> eff a b
runReader t (Cayley f) = R.runReader f t
{-# INLINE runReader #-}

writing :: w -> eff :-> (Writer w ~> eff)
writing w eff = Cayley $ W.writer (eff, w)
{-# INLINE writing #-}

mapWriter :: (w -> eff a b -> (w',eff' a' b'))
          -> (Writer w ~> eff) a b
          -> (Writer w' ~> eff') a' b'
mapWriter f (Cayley act) = Cayley $ case W.runWriter act of
  (eff,w) -> W.writer $ swap $ f w eff
{-# INLINE mapWriter #-}

mapWriter_ :: (w -> eff a b -> eff' a' b')
           -> (Writer w ~> eff) a b
           -> (Writer w ~> eff') a' b'
mapWriter_ f = mapWriter (\w e -> (w,f w e))
{-# INLINE mapWriter_ #-}

runWriter :: (Writer w ~> eff) a b -> (w, eff a b)
runWriter (Cayley eff) = swap $ W.runWriter eff
{-# INLINE runWriter #-}

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
{-# INLINE swap #-}

returning :: (Arrow eff) => b -> eff a b
returning = arr . const
{-# INLINE returning #-}

-- | Just a flipped variant of runKleisli
perform :: a -> Kleisli m a b -> m b
perform = flip runKleisli
{-# INLINE perform #-}
