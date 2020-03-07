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
import           Control.Concurrent.Async.Lifted
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Kernmantle.Arrow
import           Control.Kernmantle.Error
import           Data.Profunctor
import           Data.Profunctor.Traversing
import           Data.Profunctor.Trans
import           Prelude                         hiding (id, (.))


-- | A version of 'Kleisli' for which (***) computes in parallel
newtype PKleisli m a b = PKleisli { runPKleisli :: a -> m b }
  deriving ( Category
           , Profunctor, Strong, Choice, Closed, Costrong
           , Mapping, Traversing
           , SieveTrans m
           , ThrowEffect ex, TryEffect ex )
    via Kleisli m

instance (MonadBaseControl IO m) => Arrow (PKleisli m) where
  arr f = PKleisli (return . f)
  first (PKleisli f) = PKleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
  second (PKleisli f) = PKleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
  (PKleisli f) *** (PKleisli g) = PKleisli $ \ ~(a,b) ->
    withAsync (f a) $ \c ->
      withAsync (g b) $ \d ->
        waitBoth c d

deriving via Kleisli m instance (MonadBaseControl IO m)
  => ArrowChoice (PKleisli m)
deriving via Kleisli m instance (MonadBaseControl IO m, MonadFix m)
  => ArrowLoop (PKleisli m)
deriving via Kleisli m instance (MonadBaseControl IO m, MonadPlus m)
  => ArrowZero (PKleisli m)
deriving via Kleisli m instance (MonadBaseControl IO m, MonadPlus m)
  => ArrowPlus (PKleisli m)

-- | Just a flipped variant of runPKleisli
performP :: a -> PKleisli m a b -> m b
performP = flip runPKleisli
{-# INLINE performP #-}
