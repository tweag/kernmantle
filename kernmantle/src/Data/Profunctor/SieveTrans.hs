{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}

-- | SieveTrans exposes the SieveTrans class and some Sieve transformer based on
-- usual Reader and Writer

module Data.Profunctor.SieveTrans where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Bifunctor.Tannen
import Data.Profunctor
import Data.Profunctor.Cayley

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.Writer.Strict as W


-- | A general version of 'MonadIO' for profunctors and categories
class SieveTrans sieve cat | cat -> sieve where
  liftSieve :: sieve a b -> cat a b
  mapSieve :: (sieve a b -> sieve a' b') -> cat a b -> cat a' b'

instance SieveTrans (Star f) (Star f) where
  liftSieve = id
  mapSieve = ($)

instance SieveTrans (Kleisli m) (Kleisli m) where
  liftSieve = id
  mapSieve = ($)

instance (SieveTrans sieve cat, Applicative f)
  => SieveTrans sieve (Cayley f cat) where
  liftSieve = Cayley . pure . liftSieve
  {-# INLINE liftSieve #-}
  mapSieve f (Cayley app) = Cayley $ mapSieve f <$> app
  {-# INLINE mapSieve #-}

instance (SieveTrans sieve cat, Applicative f)
  => SieveTrans sieve (Tannen f cat) where
  liftSieve = Tannen . pure . liftSieve
  {-# INLINE liftSieve #-}
  mapSieve f (Tannen app) = Tannen $ mapSieve f <$> app
  {-# INLINE mapSieve #-}
  
type HasKleisli m = SieveTrans (Kleisli m)
type HasStar f = SieveTrans (Star f)

liftKleisli :: (HasKleisli m eff) => (a -> m b) -> eff a b
liftKleisli = liftSieve . Kleisli
{-# INLINE liftKleisli #-}

mapKleisli :: (HasKleisli m eff)
           => ((a -> m b) -> (a' -> m b')) -> eff a b -> eff a' b'
mapKleisli f = mapSieve (Kleisli . f . runKleisli)
{-# INLINE mapKleisli #-}

liftStar :: (HasStar f eff) => (a -> f b) -> eff a b
liftStar = liftSieve . Star
{-# INLINE liftStar #-}

mapStar :: (HasStar m eff)
        => ((a -> m b) -> (a -> m b)) -> eff a b -> eff a b
mapStar f = mapSieve (Star . f . runStar)
{-# INLINE mapStar #-}

type HasKleisliIO m eff = (HasKleisli m eff, MonadIO m)

liftKleisliIO :: (HasKleisliIO m eff) => (a -> IO b) -> eff a b
liftKleisliIO f = liftKleisli $ liftIO . f
{-# INLINE liftKleisliIO #-}


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
