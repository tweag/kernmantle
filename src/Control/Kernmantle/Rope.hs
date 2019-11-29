{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

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
  , FromUnary
  , type (:->)
  , (&)

  , tighten, loosen
  , entwine
  , retwine
  , untwine
  , runUnaryCore
  , mergeStrands
  , asCore
  , unary, unary_
  )
where

import Control.Category
import Control.Arrow

import Control.Monad.Reader
import Data.Bifunctor
import Data.Bifunctor.Tannen
import Data.Bifunctor.Product
import Data.Profunctor
import Data.Profunctor.Cayley
import Data.Function ((&))
import Data.Functor.Identity
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.ARec
import Data.Vinyl.TypeLevel
import GHC.Exts
import GHC.TypeLits
import GHC.OverloadedLabels

import Prelude hiding (id, (.))


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

-- | Runs one "mantle" strand (* -> * -> * effect) in a "core" strand. Is
-- parameterized over a Strand even if it ignores its name internally
-- because that's what is expect by the 'RopeRec'
newtype Weaver (core::BinEff) (strand::Strand) = Weaver
  { weaveStrand :: StrandEff strand :-> core }

-- | 'Rope' is a free arrow built out of _several_ binary effects
-- (ie. effects with kind * -> * -> *). These effects are called 'Strand's, they
-- compose the @mantle@, and they can be interlaced "on top" of an existing
-- @core@ effect.
newtype Rope (record::RopeRec) (mantle::[Strand]) (core::BinEff) a b =
  Rope
    { runRope :: record (Weaver core) mantle -> core a b }
  
  deriving (Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           ,Bifunctor)
    via Reader (record (Weaver core) mantle) `Tannen` core

  deriving (Profunctor, Strong, Choice)
    via Reader (record (Weaver core) mantle) `Cayley` core

-- | A 'Rope' over any core that satisfies some constraints.
--
-- NOTE: Given @core@ is maintained universally quantified, a 'Rope' created
-- this way corresponds to the final encoding of the free arrow construction.
type UniRope cst record mantle a b =
  forall core. (cst core) => Rope record mantle core a b

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
  strand l eff = Rope $ \r -> weaveStrand (rgetf l r) eff
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
tighten (Rope f) = Rope $ f . fromARec
{-# INLINE tighten #-}

-- | Turn a 'TightRope' into a 'LooseRope'
loosen :: (NatToInt (RLength m))
       => TightRope m core a b -> LooseRope m core a b
loosen (Rope f) = Rope $ f . toARec
{-# INLINE loosen #-}

-- | Adds a new effect strand in the 'Rope'. Users of that function should
-- normally not place constraints on the core or instanciate it. Rather,
-- requirement of the execution function should be expressed in terms of other
-- effects of the @mantle@.
entwine :: Label name  -- ^ Give a name to the strand
        -> (binEff :-> LooseRope mantle core) -- ^ The execution function
        -> LooseRope ('(name,binEff) ': mantle) core a b -- ^ The 'Rope' with an extra effect strand
        -> LooseRope mantle core a b -- ^ The rope with the extra effect strand
                                     -- woven in the core
entwine _ run (Rope f) = Rope $ \r ->
  f (Weaver (\eff -> runRope (run eff) r) :& r)
{-# INLINE entwine #-}

-- | Turns a unary effect into a binary one
type FromUnary = Kleisli

-- | Turns a function computing a unary effect to a binary effect
unary :: (a -> ueff b) -> FromUnary ueff a b
unary = Kleisli
{-# INLINE unary #-}

-- | Turns a unary effect into a binary effect
unary_ :: ueff x -> FromUnary ueff () x
unary_ = Kleisli . const
{-# INLINE unary_ #-}

-- | Runs an effect directly in the core. You should use that function only as
-- part of a call to 'entwine'.
asCore :: core x y -> Rope r mantle core x y
asCore = Rope . const
{-# INLINE asCore #-}

-- | Reorders the strands to match some external context. @strands'@ can contain
-- more elements than @strands@. Note it works on both 'TightRope's and
-- 'LooseRope's
retwine :: (RecSubset r strands strands' (RImage strands strands'), RecSubsetFCtx r (Weaver core))
        => Rope r strands core a b
        -> Rope r strands' core a b
retwine (Rope f) = Rope $ f . rcast
{-# INLINE retwine #-}

-- | Merge two strands that have the same effect type. Keeps the first name.
mergeStrands :: Label n1
             -> Label n2
             -> LooseRope ( '(n1,binEff) ': '(n2,binEff) ': mantle ) core a b
             -> LooseRope ( '(n1,binEff) ': mantle ) core a b
mergeStrands _ _ (Rope f) = Rope $ \(r@(Weaver w) :& rest) ->
  f (r :& Weaver w :& rest)
{-# INLINE mergeStrands #-}

-- | Strips a 'Rope' of its empty mantel.
untwine :: LooseRope '[] core a b -> core a b
untwine (Rope f) = f RNil
{-# INLINE untwine #-}

-- | Runs a 'Rope' with no strands left when its core is a unary effect.
runUnaryCore :: a -> LooseRope '[] (FromUnary ueff) a b -> ueff b
runUnaryCore a r = runKleisli (untwine r) a
{-# INLINE runUnaryCore #-}
