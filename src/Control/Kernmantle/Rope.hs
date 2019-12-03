{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Rope' connects together various effect 'Strand's that get interlaced
-- together.
--
-- A 'Strand' is an effet with a parameter and an output. No constraint are
-- placed on the 'Strand', but once combined in a 'Rope', that 'Rope' will be
-- an 'Arrow' and a 'Profunctor'. 'Strand's in a 'Rope' are named via labels to
-- limit ambiguity.
--
-- An action that targets some 'Strand' can be lifted to a 'Rope' that contains
-- that 'Strand' with the 'strand' function.

module Control.Kernmantle.Rope
  ( Product(..)
  , Tannen(..)
  , Kleisli(..)
  , Rope(..)
  , TightRope, LooseRope
  , BinEff, Strand, RopeRec
  , StrandName, StrandEff
  , Weaver(..)
  , InRope(..)
  , AnyRopeWith
  , Entwines, SatisfiesAll
  , Label, fromLabel
  , ToSieve
  , WithAoT
  , type (:->)
  , (&)

  , tighten, loosen
  , entwine
  , retwine
  , untwine
  , runSieveCore
  , mergeStrands
  , asCore
  , toSieve, toSieve_

  , withAoT
  )
where

import Control.Category
import Control.Arrow
import Control.Monad.Trans.Reader
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Bifunctor.Product
import Data.Profunctor hiding (rmap)
import Data.Profunctor.Cayley
import Data.Profunctor.Sieve
import Data.Function ((&))
import Data.Functor.Identity
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.ARec
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.Exts
import GHC.TypeLits
import GHC.OverloadedLabels

import Prelude hiding (id, (.))

import Control.Kernmantle.Error
import Control.Kernmantle.Functors


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

newtype RopeRunner (record::RopeRec) (mantle::[Strand]) (interp::BinEff) (core::BinEff) a b =
  RopeRunner (record (Weaver interp) mantle -> core a b)
  
  deriving ( Category, Bifunctor
           , Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Closed, Costrong, Cochoice
           , ThrowEffect ex, TryEffect ex
           )
    via Reader (record (Weaver interp) mantle) `Tannen` core

  deriving (Profunctor, Strong, Choice)
    via Reader (record (Weaver interp) mantle) `Cayley` core

instance (RMap m) => EffProfunctor (RopeRunner Rec m) where
  effdimap f g (RopeRunner run) = RopeRunner $
    g . run . rmap (mapWeaverInterp f)
  {-# INLINE effdimap #-}

-- | 'Rope' is a free arrow built out of _several_ binary effects (ie. effects
-- with kind * -> * -> *). These effects are called 'Strand's, they compose the
-- @mantle@, can be interpreted in an @interp@ effect and can be interlaced "on
-- top" of an existing @core@ effect.
newtype Rope record mantle core a b = Rope (RopeRunner record mantle core core a b)
  deriving ( Category, Bifunctor
           , Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Closed, Costrong, Cochoice
           , ThrowEffect ex, TryEffect ex
           , Profunctor, Strong, Choice
           )

runRope :: Rope record mantle core a b -> record (Weaver core) mantle -> core a b
runRope (Rope (RopeRunner f)) = f
{-# INLINE runRope #-}

mkRope :: (record (Weaver core) mantle -> core a b) -> Rope record mantle core a b
mkRope = Rope . RopeRunner
{-# INLINE mkRope #-}

-- | A 'Rope' that is "tight", meaning you cannot 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(1)@ on 'TightRope's whatever the number of
-- 'Strand's.
type TightRope = Rope ARec

-- | A 'Rope' that is "loose", meaning you can 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(n)@ on 'LooseRope's, @n@ being the number of
-- 'Strand's.
type LooseRope = Rope Rec

class InRope l eff rope where
  -- | Lifts a binary effect in the 'Rope'. Performance should be better with a
  -- 'TightRope' than with a 'LooseRope', unless you have very few 'Strand's.
  strand :: Label l -> eff a b -> rope a b

instance ( HasField record l mantle mantle eff eff
         , RecElemFCtx record (Weaver core) )
  => InRope l eff (Rope record mantle core) where
  strand l eff = mkRope $ \r -> weaveStrand (rgetf l r) eff
  {-# INLINE strand #-}

-- | Tells whether a collection of @strands@ is in a 'Rope'
type family rope `Entwines` (strands::[Strand]) :: Constraint where
  rope `Entwines` '[] = ()
  rope `Entwines` ('(name, eff) ': strands ) = ( InRope name eff rope
                                               , rope `Entwines` strands )

type family (x::k) `SatisfiesAll` (csts::[k -> Constraint]) :: Constraint where
  x `SatisfiesAll` '[] = ()
  x `SatisfiesAll` (c1 ': cnsts) = ( c1 x, x `SatisfiesAll` cnsts)

-- | A 'Rope' with some requirements on the strands, though not on their
-- ordering, and on the core
type AnyRopeWith strands coreConstraints a b = forall s r c.
  (Rope r s c `Entwines` strands, c `SatisfiesAll` coreConstraints)
  => Rope r s c a b

-- | Turn a 'LooseRope' into a 'TightRope'
tighten :: (RecApplicative m, RPureConstrained (IndexableField m) m)
        => LooseRope m core a b -> TightRope m core a b
tighten r = mkRope $ runRope r . fromARec
{-# INLINE tighten #-}

-- | Turn a 'TightRope' into a 'LooseRope'
loosen :: (NatToInt (RLength m))
       => TightRope m core a b -> LooseRope m core a b
loosen r = mkRope $ runRope r . toARec
{-# INLINE loosen #-}

-- | Adds a new effect strand in the 'Rope'. Users of that function should
-- normally not place constraints on the core or instanciate it. Rather,
-- requirement of the execution function should be expressed in terms of other
-- effects of the @mantle@.
entwine :: Label name  -- ^ Give a name to the strand
        -> (binEff :-> LooseRope mantle core) -- ^ The execution function
        -> LooseRope ('(name,binEff) ': mantle) core a b -- ^ The 'Rope' with an extra effect strand
        -> LooseRope mantle core a b -- ^ The 'Rope' with the extra effect strand
                                     -- woven in the core
entwine _ run rope = mkRope $ \r ->
  runRope rope (Weaver (\eff -> runRope (run eff) r) :& r)
{-# INLINE entwine #-}

-- | Runs an effect directly in the core. You should use that function only as
-- part of a call to 'entwine'.
asCore :: core x y -> Rope r mantle core x y
asCore = mkRope . const
{-# INLINE asCore #-}

-- | Indicates that @strands@ can be reordered and considered a subset of
-- @strands'@
type RetwinableAs record strands core strands' =
  ( RecSubset record strands strands' (RImage strands strands')
  , RecSubsetFCtx record (Weaver core) )

-- | Reorders the strands to match some external context. @strands'@ can contain
-- more elements than @strands@. Note it works on both 'TightRope's and
-- 'LooseRope's
retwine :: (RetwinableAs r strands core strands')
        => Rope r strands core a b
        -> Rope r strands' core a b
retwine r = mkRope $ runRope r . rcast
{-# INLINE retwine #-}

-- | Splits a 'RopeRunner' in two parts, so we can select several strands to act on
splitRopeRunner :: RopeRunner Rec (mantle1 ++ mantle2) interp core a b
                -> RopeRunner Rec mantle1 interp (RopeRunner Rec mantle2 interp core) a b
splitRopeRunner (RopeRunner f) = RopeRunner $ \r1 -> RopeRunner $ \r2 ->
  f $ r1 `rappend` r2

joinRopeRunner :: (RetwinableAs r mantle1 interp mantle
                  ,RetwinableAs r mantle2 interp mantle)
               => RopeRunner r mantle1 interp (RopeRunner r mantle2 interp core) a b
               -> RopeRunner r mantle interp core a b
joinRopeRunner (RopeRunner f) = RopeRunner $ \r -> case f (rcast r) of
  RopeRunner f' -> f' (rcast r)

-- -- API-wise, it'd be better if splitRope could be reformulated as the
-- -- following, but that seems harder to implement:
-- groupStrands :: LooseRope (mantle1 ++ mantle2) core a b
--              -> LooseRope ('(name,LooseRope mantle1 core) ': mantle2) core a b
-- groupStrands (Rope f) = Rope $ \(Weaver subrope :& r) ->  

-- | Merge two strands that have the same effect type. Keeps the first name.
mergeStrands :: Label n1
             -> Label n2
             -> LooseRope ( '(n1,binEff) ': '(n2,binEff) ': mantle ) core a b
             -> LooseRope ( '(n1,binEff) ': mantle ) core a b
mergeStrands _ _ rope = mkRope $ \(r@(Weaver w) :& rest) ->
  runRope rope (r :& Weaver w :& rest)
{-# INLINE mergeStrands #-}

-- | Strips a 'Rope' of its empty mantel.
untwine :: LooseRope '[] core a b -> core a b
untwine r = runRope r RNil
{-# INLINE untwine #-}


-- * Creating and running binary effects from unary effects

-- | Runs a 'Rope' with no strands left when its core is a 'Sieve' (ie. is
-- mappable to a functorial/monadic effect)
runSieveCore :: (Sieve biEff uEff)
             => a -> LooseRope '[] biEff a b -> uEff b
runSieveCore a r = sieve (untwine r) a
{-# INLINE runSieveCore #-}

-- | A 'Sieve' is a binary effect constructed from a functorial (or monadic)
-- unary effect.
type ToSieve = Kleisli

-- | Turns a function computing a functorial/monadic effect to a binary effect
toSieve :: (a -> ueff b) -> ToSieve ueff a b
toSieve = Kleisli
{-# INLINE toSieve #-}

-- | Turns a functorial/monadic effect into a binary effect
toSieve_ :: ueff x -> ToSieve ueff () x
toSieve_ = Kleisli . const
{-# INLINE toSieve_ #-}

-- | Wrap each strand effect inside a type constructor
type family MapStrandEffs f mantle where
  MapStrandEffs f '[] = '[]
  MapStrandEffs f ( s ': strands ) = '(StrandName s, f (StrandEff s)) ': MapStrandEffs f strands

effmapRopeRec :: (EffFunctor f)
              => Rec (Weaver interp) strands
              -> Rec (Weaver (f interp)) (MapStrandEffs f strands)
effmapRopeRec RNil = RNil
effmapRopeRec (Weaver w :& rest) = Weaver (effmap w) :& effmapRopeRec rest

-- | When all the strands of a 'Rope' have the same type of ahead of time
-- effects, we can run them. See 'splitRope' to isolate some strands in a 'Rope'
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
                     -- ^ The 'RopeRunner' to transform, where first strands all
                     -- are wrapped in the @f@ EffFunctor
                 :-> RopeRunner Rec mantle1                              core'  core'
                     -- ^ The resulting 'RopeRunner', where 
unwrapSomeStrands f g = unwrapRopeRunner . effdimap f g . splitRopeRunner
{-# INLINE unwrapSomeStrands #-}

-- entwineEffFunctors :: (EffFunctor f, RMap (MapStrandEffs f mantle1))
--                    => RopeRunner Rec (MapStrandEffs f mantle1 ++ mantle2) interp core
--                   :-> RopeRunner Rec mantle1                              core'  core'
-- entwineEffFunctors = unwrapSomeStrands id id
