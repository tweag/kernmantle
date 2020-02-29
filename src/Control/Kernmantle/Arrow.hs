{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Helper functions for Arrow effects

module Control.Kernmantle.Arrow where

import Control.Arrow
import Control.Category
import Data.List (uncons)
import Data.Profunctor
import Data.Profunctor.Cayley
import Data.Profunctor.SieveTrans
import Data.Profunctor.Traversing
import GHC.Generics

import Control.Kernmantle.Error

import Prelude hiding ((.), id)


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

-- | The identifiers reflect the structure of the pipeline that led to some
-- task. They are increased at every use of (.), (***), (|||) or (<+>). This
-- makes it so every task in the pipeline has a different identifier.
data ArrowIdent = ArrowIdent
  { aidComp   :: {-# UNPACK #-} !Int
  , aidPar    :: {-# UNPACK #-} !Int
  , aidPlus   :: {-# UNPACK #-} !Int
  , aidChoice :: {-# UNPACK #-} !Int }
  deriving (Generic, Show)

-- | An arrow transformer that can automatically determine an identifier from
-- its position in a pipeline. It is isomorphic to a @Reader ArrowIdent ~> arr@, but
-- we need a different Arrow instance than what 'Cayley' provides.
newtype AutoIdent arr a b = AutoIdent (ArrowIdent -> arr a b)
  deriving (Profunctor, Strong, Choice)
    via WrappedArrow (AutoIdent arr)

-- | Given an initial ArrowIdent, gets the wrapped effect
runAutoIdent :: ArrowIdent -> AutoIdent arr a b -> arr a b
runAutoIdent i (AutoIdent f) = f i

instance (Category eff) => Category (AutoIdent eff) where
  id = AutoIdent $ const id
  AutoIdent b . AutoIdent a = AutoIdent $ \aid ->
    b aid{aidComp=aidComp aid + 1} . a aid
instance (Arrow eff) => Arrow (AutoIdent eff) where
  arr = AutoIdent . const . arr
  first (AutoIdent f) = AutoIdent $ first . f
  second (AutoIdent f) = AutoIdent $ second . f
  AutoIdent a *** AutoIdent b = AutoIdent $ \aid ->
    a aid *** b aid{aidPar=aidPar aid + 1}
  AutoIdent a &&& AutoIdent b = AutoIdent $ \aid ->
    a aid &&& b aid{aidPar=aidPar aid + 1}
instance (ArrowZero eff) => ArrowZero (AutoIdent eff) where
  zeroArrow = AutoIdent $ const zeroArrow
instance (ArrowPlus eff) => ArrowPlus (AutoIdent eff) where
  AutoIdent a <+> AutoIdent b = AutoIdent $ \aid ->
    a aid <+> b aid{aidPlus=aidPlus aid + 1}
instance (ArrowChoice eff) => ArrowChoice (AutoIdent eff) where
  left (AutoIdent f) = AutoIdent $ left . f
  right (AutoIdent f) = AutoIdent $ right . f
  AutoIdent a ||| AutoIdent b = AutoIdent $ \aid ->
    a aid ||| b aid{aidChoice=aidChoice aid + 1}
  AutoIdent a +++ AutoIdent b = AutoIdent $ \aid ->
    a aid +++ b aid{aidChoice=aidChoice aid + 1}

instance (Traversing eff, ArrowChoice eff) => Traversing (AutoIdent eff) where
  traverse' (AutoIdent a) = AutoIdent $ traverse' . a
  wander f (AutoIdent a) = AutoIdent $ wander f . a

instance (SieveTrans s eff) => SieveTrans s (AutoIdent eff) where
  liftSieve = AutoIdent . const . liftSieve
  mapSieve f (AutoIdent af) = AutoIdent $ mapSieve f . af

-- | All effects that internally feature some AutoIdent
class HasAutoIdent wrappedEff eff | eff -> wrappedEff where
  liftAutoIdent :: (ArrowIdent -> wrappedEff a b) -> eff a b

instance HasAutoIdent eff (AutoIdent eff) where
  liftAutoIdent f = AutoIdent f

instance (HasAutoIdent i eff, Applicative f)
  => HasAutoIdent i (Cayley f eff) where
  liftAutoIdent f = Cayley $ pure (liftAutoIdent f)
