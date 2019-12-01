{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}

-- | Helper functions for Arrow effects

module Control.Kernmantle.Arrow where

import Control.Arrow
import Data.List (uncons)

import Control.Kernmantle.Error


-- | Map an arrow over a list using (***)
parMapA :: ArrowChoice a => a b c -> a [b] [c]
parMapA f = arr (maybe (Left ()) Right . uncons)
      >>> (arr (const []) ||| ((f *** parMapA f) >>> arr (uncurry (:))))

-- | Map an arrow over a list using (>>>) and 'first'
seqMapA :: ArrowChoice a => a b c -> a [b] [c]
seqMapA f = arr (maybe (Left ()) Right . uncons)
            >>> (arr (const []) ||| ((first f >>> second (seqMapA f))
                                     >>> arr (uncurry (:))))

-- | Repeats an arrow step in order to fold a list
foldlA :: ArrowChoice a => a (b,acc) acc -> a ([b],acc) acc
foldlA f = proc (input,acc) ->
  case input of
    [] -> returnA -< acc
    (x:xs) -> do
      !acc' <- f -< (x,acc)
      foldlA f -< (xs,acc')

-- | Filter a list given an arrow filter
filterA :: ArrowChoice a => a b Bool -> a [b] [b]
filterA f = proc xs ->
  case xs of
    [] -> returnA -< []
    (y:ys) -> do
      b <- f -< y
      if b then
        (second (filterA f) >>> arr (uncurry (:))) -< (y,ys)
      else
        filterA f -< ys

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
