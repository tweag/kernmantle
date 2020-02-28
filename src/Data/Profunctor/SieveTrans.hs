{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}

module Data.Profunctor.SieveTrans where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Bifunctor.Tannen
import Data.Profunctor
import Data.Profunctor.Cayley


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


-- | An equivalent of ($) over type applications
type (~>) (f::k->k') (eff::k) = f eff
infixr 1 ~>  -- To be of a lower precedence than (:->)

type Using = Cayley
type Given a = Using ((->) a)
type Collecting a = Using ((,) a)
type Performing = Kleisli

skipping :: (Applicative f) => eff :-> Using f eff
skipping = Cayley . pure
{-# INLINE skipping #-}

using :: (Functor f) => f t -> (t -> eff a b) -> Using f eff a b
using a f = Cayley $ fmap f a
{-# INLINE using #-}

use :: Cayley f eff a b -> f (eff a b)
use (Cayley eff) = eff
{-# INLINE use #-}

mapUsing :: (Functor f) => (eff a b -> eff' a' b') -> Using f eff a b -> Using f eff' a' b'
mapUsing f (Cayley eff) = Cayley $ fmap f eff
{-# INLINE mapUsing #-}

given :: (t -> eff a b) -> (Given t eff) a b
given = Cayley
{-# INLINE given #-}

mapGiven :: (t -> eff a b -> eff' a' b')
         -> Given t eff a b
         -> Given t eff' a' b'
mapGiven f (Cayley eff) = Cayley (\x -> f x $ eff x)
{-# INLINE mapGiven #-}

give :: t -> Given t eff a b -> eff a b
give t (Cayley f) = f t
{-# INLINE give #-}

collecting :: w -> eff :-> Collecting w eff
collecting w eff = Cayley (w, eff)
{-# INLINE collecting #-}

mapCollecting :: (w -> eff a b -> eff' a' b')
              -> Collecting w eff a b
              -> Collecting w eff' a' b'
mapCollecting f (Cayley (w,eff)) = Cayley (w,f w eff)
{-# INLINE mapCollecting #-}

collect :: Collecting w eff a b -> (w, eff a b)
collect (Cayley t) = t
{-# INLINE collect #-}

type Performs m = HasKleisli m

performing :: (Performs m eff) => (a -> m b) -> eff a b
performing = liftKleisli
{-# INLINE performing #-}

returning :: (Arrow eff) => b -> eff a b
returning = arr . const
{-# INLINE returning #-}

mapPerforming :: (Performs m eff)
              => ((a -> m b) -> (a' -> m b')) -> eff a b -> eff a' b'
mapPerforming = mapKleisli
{-# INLINE mapPerforming #-}

perform :: a -> Performing m a b -> m b
perform = flip runKleisli
{-# INLINE perform #-}
