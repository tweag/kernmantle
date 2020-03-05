{-# LANGUAGE FlexibleContexts #-}

module Control.Kernmantle.Streams where

import Control.Kernmantle.Rope
import Streaming
import qualified Streaming.Prelude as S


mapStream :: (HasKleisli m core, Monad m)
          => Rope r mt core a b
          -> Rope r mt core (Stream (Of a) m x) (Stream (Of b) m x)
mapStream = mapKleisli (\f -> return . S.mapM f)
{-# INLINE mapStream #-}
