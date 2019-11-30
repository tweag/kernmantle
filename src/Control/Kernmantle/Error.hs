{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Arrows #-}

-- | Deal with throw and try effects

module Control.Kernmantle.Error
  (ThrowEffect(..), TryEffect(..), catchE
  ,module Control.Exception.Safe)
where

import Control.Applicative
import Control.Arrow
import Control.Exception.Safe
import Data.Bifunctor.Tannen

  
-- | A class for binary effects that can possibly throw exceptions
class ThrowEffect ex eff where
  throwE :: eff (Either ex b) b

-- | A class for binary effects that can catch exceptions
class TryEffect ex eff where
  tryE :: eff a b -> eff a (Either ex b)

instance (MonadThrow m, Exception ex) => ThrowEffect ex (Kleisli m) where
  throwE = Kleisli $ either throw return
  {-# INLINE throwE #-}

instance (MonadCatch m, Exception ex) => TryEffect ex (Kleisli m) where
  tryE (Kleisli act) = Kleisli $ try . act
  {-# INLINE tryE #-}

instance (Applicative f, ThrowEffect ex eff) => ThrowEffect ex (f `Tannen` eff) where
  throwE = Tannen $ pure throwE
  {-# INLINE throwE #-}

instance (Functor f, TryEffect ex eff) => TryEffect ex (f `Tannen` eff) where
  tryE (Tannen f) = Tannen $ tryE <$> f
  {-# INLINE tryE #-}

-- | If a 'TryEffect' is also an 'ArrowChoice', then we can implement catch
catchE :: (TryEffect ex eff, ArrowChoice eff)
       => eff e c  -- ^ The effect to wrap
       -> eff (e, ex) c  -- ^ What to do in case of exception
       -> eff e c
catchE a onExc = proc e -> do
  res <- tryE a -< e
  case res of
    Left ex ->
      onExc -< (e, ex)
    Right r ->
      returnA -< r
