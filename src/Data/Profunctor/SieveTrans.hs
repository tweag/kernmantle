{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}

module Data.Profunctor.SieveTrans where

import Control.Arrow
import Control.Monad.IO.Class
import Data.Bifunctor.Tannen
import Data.Profunctor.Cayley
import Data.Profunctor


-- | A general version of 'MonadIO' for profunctors and categories
class SieveTrans sieve cat | cat -> sieve where
  liftSieve :: sieve a b -> cat a b

instance SieveTrans (Star f) (Star f) where
  liftSieve = id

instance SieveTrans (Kleisli m) (Kleisli m) where
  liftSieve = id

instance (SieveTrans sieve cat, Applicative f)
  => SieveTrans sieve (Cayley f cat) where
  liftSieve = Cayley . pure . liftSieve

instance (SieveTrans sieve cat, Applicative f)
  => SieveTrans sieve (Tannen f cat) where
  liftSieve = Tannen . pure . liftSieve

type HasKleisli m = SieveTrans (Kleisli m)
type HasStar f = SieveTrans (Star f)

liftKleisli :: (HasKleisli m eff) => (a -> m b) -> eff a b
liftKleisli = liftSieve . Kleisli
{-# INLINE liftKleisli #-}

liftStar :: (HasStar f eff) => (a -> f b) -> eff a b
liftStar = liftSieve . Star
{-# INLINE liftStar #-}

type HasMonadIO eff m = (MonadIO m, HasKleisli m eff)

liftKleisliIO :: (HasMonadIO eff m) => (a -> IO b) -> eff a b
liftKleisliIO f = liftKleisli $ liftIO . f
{-# INLINE liftKleisliIO #-}
