{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Kernmantle.Parallel where

import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Monad.Fix
import           Control.Kernmantle.Arrow
import           Control.Kernmantle.Error
import           Data.Profunctor
import           Data.Profunctor.Mapping
import           Data.Profunctor.Traversing
import           Data.Profunctor.Trans
import           Prelude                         hiding (id, (.))
import           UnliftIO


-- | A version of 'Kleisli' for which (***) and traverse' compute in parallel
newtype PKleisli m a b = PKleisli { runPKleisli :: a -> m b }
  deriving ( Category
           , Profunctor, Strong, Choice, Costrong
           , SieveTrans m
           , ThrowEffect ex, TryEffect ex )
    via Kleisli m

instance (MonadUnliftIO m) => Arrow (PKleisli m) where
  arr f = PKleisli (return . f)
  first (PKleisli f) = PKleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (PKleisli f) = PKleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
  (PKleisli f) *** (PKleisli g) = PKleisli $ \ ~(a,b) -> concurrently (f a) (g b)

deriving via Kleisli m instance (MonadUnliftIO m)
  => ArrowChoice (PKleisli m)
deriving via Kleisli m instance (MonadUnliftIO m, MonadFix m)
  => ArrowLoop (PKleisli m)
deriving via Kleisli m instance (MonadUnliftIO m, MonadPlus m)
  => ArrowZero (PKleisli m)
deriving via Kleisli m instance (MonadUnliftIO m, MonadPlus m)
  => ArrowPlus (PKleisli m)

instance (MonadUnliftIO m) => Traversing (PKleisli m) where
  -- TODO: test wander to see if it really happens in parallel
  traverse' (PKleisli f) = PKleisli $ mapConcurrently f

instance (MonadUnliftIO m) => Mapping (PKleisli m) where
  map' = traverseMapping

instance (MonadUnliftIO m) => Closed (PKleisli m) where
  closed = closedMapping

-- | Just a flipped variant of runPKleisli
performP :: a -> PKleisli m a b -> m b
performP = flip runPKleisli
{-# INLINE performP #-}
