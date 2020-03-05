{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

-- | Deal with throw and try effects

module Control.Kernmantle.Error
  (ThrowEffect(..), TryEffect(..)
  ,module Control.Exception.Safe)
where

import Control.Applicative
import Control.Arrow
import Control.Exception.Safe
import Data.Profunctor.Cayley

  
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

instance (Applicative f, ThrowEffect ex eff) => ThrowEffect ex (f `Cayley` eff) where
  throwE = Cayley $ pure throwE
  {-# INLINE throwE #-}

instance (Functor f, TryEffect ex eff) => TryEffect ex (f `Cayley` eff) where
  tryE (Cayley f) = Cayley $ tryE <$> f
  {-# INLINE tryE #-}
