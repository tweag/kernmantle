{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Control.Kernmantle.Rope.Internal where

import Control.Category
import Control.Arrow
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Profunctor hiding (rmap)
import Data.Bifunctor hiding (second)
import Data.Biapplicative hiding (second)
import Data.Bifunctor.Tannen
import Data.Functor.Identity
import Data.Profunctor.Cayley
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.TypeLevel
import GHC.TypeLits

import Prelude hiding (id, (.))

import Control.Kernmantle.Error
import Control.Kernmantle.Builder


-- | The kind for all binary effects. First param is usually an input
-- (contravariant) of the effect and second one an output (covariant).
type BinEff = * -> * -> *

-- | The kind for unary effects
type UnaryEff = * -> *

-- | The kind for a named binary effect. Must remain a tuple because that's what
-- vinyl expects.
type Strand = (Symbol, BinEff)

type family StrandName t where
  StrandName '(name, eff) = name

type family StrandEff t where
  StrandEff '(name, eff) = eff

-- | The kind for records that will contain 'Weaver's. First type param will
-- most often be @Weaver someCore@
type RopeRec = (Strand -> *) -> [Strand] -> *

-- | Runs one @strand@ (* -> * -> * effect) in a @interp@ effect. Is
-- parameterized over a Strand (and not just a BinEffect) even if it ignores its
-- name internally because that's what is expect by the 'RopeRec'
newtype Weaver (interp::BinEff) (strand::Strand) = Weaver
  { weaveStrand :: StrandEff strand :-> interp }

mapWeaverInterp :: (interp :-> interp')
                -> Weaver interp strand
                -> Weaver interp' strand
mapWeaverInterp f (Weaver w) = Weaver $ f . w
{-# INLINE mapWeaverInterp #-}

-- | The internal implementation of a Rope, where @interp@ and @core@ can be two
-- different types (this facilitates implementation of some functions).
--
-- @record@ holds the functions to interpret the @strands@ in an @interp@
-- effect. And then all these interpreted effects will run in a @core@ effect.
newtype RopeRunner (record::RopeRec) (mantle::[Strand]) (interp::BinEff) (core::BinEff) a b =
  RopeRunner (record (Weaver interp) mantle -> core a b)
  
  deriving ( Bifunctor, Biapplicative
           , Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Closed, Costrong, Cochoice
           , ThrowEffect ex, TryEffect ex
           , StatefulEff
           )
    via Tannen ((->) (record (Weaver interp) mantle)) core

  deriving (EffFunctor, EffPointedFunctor)
    via Tannen ((->) (record (Weaver interp) mantle))

  deriving (Profunctor, Strong, Choice)
    via Cayley ((->) (record (Weaver interp) mantle)) core

instance (RMap m) => EffProfunctor (RopeRunner Rec m) where
  effdimap f g (RopeRunner run) = RopeRunner $
    g . run . rmap (mapWeaverInterp f)
  {-# INLINE effdimap #-}

-- | Splits a 'RopeRunner' in two parts, so we can select several strands to act on
splitRopeRunner :: RopeRunner Rec (mantle1 ++ mantle2) interp core
               :-> RopeRunner Rec mantle1 interp (RopeRunner Rec mantle2 interp core)
splitRopeRunner (RopeRunner f) = RopeRunner $ \r1 -> RopeRunner $ \r2 ->
  f $ r1 `rappend` r2

-- | Indicates that @strands@ can be reordered and considered a subset of
-- @strands'@
type RetwinableAs record strands core strands' =
  ( RecSubset record strands strands' (RImage strands strands')
  , RecSubsetFCtx record (Weaver core) )

joinRopeRunner :: (RetwinableAs r mantle1 interp mantle
                  ,RetwinableAs r mantle2 interp mantle)
               => RopeRunner r mantle1 interp (RopeRunner r mantle2 interp core)
              :-> RopeRunner r mantle interp core
joinRopeRunner (RopeRunner f) = RopeRunner $ \r -> case f (rcast r) of
  RopeRunner f' -> f' (rcast r)

-- | Wrap each strand effect inside a type constructor
type family MapStrandEffs f mantle where
  MapStrandEffs f '[] = '[]
  MapStrandEffs f ( s ': strands ) = '(StrandName s, f (StrandEff s)) ': MapStrandEffs f strands

effmapRopeRec :: (EffFunctor f)
              => Rec (Weaver interp) strands
              -> Rec (Weaver (f interp)) (MapStrandEffs f strands)
effmapRopeRec RNil = RNil
effmapRopeRec (Weaver w :& rest) = Weaver (effrmap w) :& effmapRopeRec rest

-- | When all the strands of a 'Rope' are built by the same 'EffFunctor', we can
-- run them. See 'splitRope' to isolate some strands in a 'Rope'
unwrapRopeRunner :: (EffFunctor f)
                 => RopeRunner Rec (MapStrandEffs f strands) (f core) core
                :-> RopeRunner Rec strands core core
unwrapRopeRunner (RopeRunner f) = RopeRunner $ f . effmapRopeRec
{-# INLINE unwrapRopeRunner #-}

unwrapSomeStrands :: (EffFunctor f, RMap (MapStrandEffs f mantle1))
                  => (f core' :-> interp)
                     -- ^ How to run the extracted EffFunctor layer
                  -> (RopeRunner Rec mantle2 interp core :-> core')
                     -- ^ What to do with the remaining strands (those not
                     -- wrapped)
                  -> RopeRunner Rec (MapStrandEffs f mantle1 ++ mantle2) interp core
                 :-> RopeRunner Rec mantle1                              core'  core'
                     -- ^ The resulting 'RopeRunner', where 
unwrapSomeStrands f g = unwrapRopeRunner . effdimap f g . splitRopeRunner
{-# INLINE unwrapSomeStrands #-}

-- | Change the state type of a 'StatefulEff'
type family ChangeEffState (eff::BinEff) s' :: BinEff

-- | A class for effects which can pass a state around and modify it
class StatefulEff eff where
  type EffState eff
  withEffState :: eff (a,EffState eff) (b,EffState eff) -> eff a b
  changeEffState :: ChangeEffState eff s' a b -> eff (a,s') (b,s')

-- | Locally change the state of an effect to some @s'@
mapEffState :: (StatefulEff eff, Arrow eff)
            => (EffState eff -> s')
            -> (s' -> EffState eff)
            -> ChangeEffState eff s' a b
            -> eff a b
mapEffState to from eff = withEffState $
  second (arr to) >>> changeEffState eff >>> second (arr from)

type instance ChangeEffState (Tannen f eff) s' = Tannen f (ChangeEffState eff s')

instance (Functor f, StatefulEff eff) => StatefulEff (Tannen f eff) where
  type EffState (Tannen f eff) = EffState eff
  withEffState (Tannen f) = Tannen $ withEffState <$> f
  changeEffState (Tannen f) = Tannen $ changeEffState <$> f

type instance ChangeEffState (Kleisli (StateT s m)) s' = Kleisli (StateT s' m)

instance (Functor m) => StatefulEff (Kleisli (StateT s m)) where
  type EffState (Kleisli (StateT s m)) = s
  withEffState (Kleisli fn) = Kleisli $ \a -> StateT $ \s -> do
    fst <$> runStateT (fn (a,s)) s
  changeEffState (Kleisli fn) = Kleisli $ \(a,s') -> StateT $ \s -> do
    (,s) <$> runStateT (fn a) s'

type instance ChangeEffState (RopeRunner r m i c) s' = RopeRunner r m i (ChangeEffState c s')
