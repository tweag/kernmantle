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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Arrows #-}

module Control.Kernmantle.Rope.Internal where

import Control.Category
import Control.Arrow
import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Profunctor hiding (rmap)
import Data.Biapplicative
import Data.Bifunctor.Tannen
import Data.Functor.Identity
import Data.Profunctor.Cayley
import Data.Profunctor.Monad
import Data.Profunctor.Traversing
import Data.Profunctor.SieveTrans
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

  deriving ( Category
           , Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Profunctor, Strong, Choice, Closed, Costrong, Cochoice
           , Mapping, Traversing
           , ThrowEffect ex, TryEffect ex
           , SieveTrans sieve )
    via Cayley ((->) (record (Weaver interp) mantle)) core
  deriving (ProfunctorFunctor, ProfunctorMonad)
    via Cayley ((->) (record (Weaver interp) mantle))

  deriving (Bifunctor, Biapplicative, ArrowState s)
    via Tannen ((->) (record (Weaver interp) mantle)) core
  deriving (EffFunctor, EffPointedFunctor)
    via Tannen ((->) (record (Weaver interp) mantle))

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

--------------------------------------------------------------------------------
-- State

-- | A class for effects which can pass around a mutable state.
class Arrow eff => ArrowState s eff | eff -> s where
  {-# MINIMAL stateA | getA, putA #-}
  stateA :: eff (a,s) (b,s) -> eff a b
  stateA eff = proc a -> do
    s <- getA -< ()
    (b,s') <- eff -< (a,s)
    putA -< s'
    returnA -< b

  getA :: eff () s
  getA = stateA $ arr (\((),s) -> (s,s))

  putA :: eff s ()
  putA = stateA $ arr (\(_,s) -> ((),s))

instance (Applicative f, ArrowState s eff) => ArrowState s (Tannen f eff) where
  stateA (Tannen f) = Tannen $ stateA <$> f

instance (MonadState s m) => ArrowState s (Kleisli m) where
  stateA (Kleisli f) = Kleisli $ \a -> do
    s <- get
    (b,s') <- f (a,s)
    put s'
    return b

  getA = Kleisli (const get)

  putA = Kleisli put
