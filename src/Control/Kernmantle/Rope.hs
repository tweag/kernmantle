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
  , IOStrand
  , InRope(..), Entwines
  , Label, fromLabel
  , type (~>)
  , (&)

  , tighten, loosen
  , untwine
  , entwine

  , untwineIO
  , ioStrand, ioStrand_
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

-- | The kind for a named binary effect. Must remain a tuple because that's what
-- vinyl expects.
type Strand = (Symbol, BinEff)

type family StrandName (t::Strand) where
  StrandName '(name, eff) = name

type family StrandEff (t::Strand) where
  StrandEff '(name, eff) = eff

-- | The kind for records that will contain 'Weaver's. First type param will
-- most often be @Weaver someCore@
type RopeRec = (Strand -> *) -> [Strand] -> *

-- | A natural transformation on type constructors of two arguments.
type f ~> g = forall x y. f x y -> g x y

-- | Runs one "mantle" strand (* -> * -> * effect) in a "core" strand. Is
-- parameterized over a Strand even if it ignores its name internally
-- because that's what is expect by the 'RopeRec'
newtype Weaver (core::BinEff) (strand::Strand) = Weaver
  { weaveStrand :: StrandEff strand ~> core }

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

-- type IsInRope (record::RopeRec) (strands::[Strand]) (core::BinEff) (strand::Strand) =
--   ( HasField record (StrandName strand) strands strands (Snd strand) (Snd strand)
--   , RecElemFCtx record (Weaver core) )

class InRope l eff rope where
  -- | Lifts an effect in the 'Rope'. Performance should be better with a
  -- 'TightRope' than with a 'LooseRope', unless you have very few 'Strand's.
  strand :: Label l -> eff a b -> rope a b

instance ( HasField record l mantle mantle eff eff
         , RecElemFCtx record (Weaver core) )
  => InRope l eff (Rope record mantle core) where
  strand l eff = Rope $ \r -> weaveStrand (rgetf l r) eff
  {-# INLINE strand #-}

-- | Tells whether a collection of @strands@ is in a 'Rope'
type family rope `Entwines` strands :: Constraint where
  rope `Entwines` '[] = ()
  rope `Entwines` ('(name, eff) ': strands ) = ( InRope name eff rope
                                               , rope `Entwines` strands )

-- | Turn a 'LooseRope' into a 'TightRope'
tighten :: (RecApplicative m, RPureConstrained (IndexableField m) m)
        => LooseRope m core a b -> TightRope m core a b
tighten (Rope f) = Rope $ f . fromARec

-- | Turn a 'TightRope' into a 'LooseRope'
loosen :: (NatToInt (RLength m))
       => TightRope m core a b -> LooseRope m core a b
loosen (Rope f) = Rope $ f . toARec

-- | Adds a new effect strand to the mantle of the 'Rope'. Users of that
-- function should normally not place constraints on the core or instanciate
-- it. Rather, requirement of the execution function should be expressed in
-- terms of other effects of the @mantle@.
entwine :: Label name
        -> (binEff ~> LooseRope mantle core) -- ^ The execution function
        -> LooseRope ('(name,binEff) ': mantle) core a b -- ^ The 'Rope' with an extra effect strand
        -> LooseRope mantle core a b -- ^ The rope with the extra effect strand
                                     -- woven in the core
entwine _ run (Rope f) = Rope $ \r ->
  f (Weaver (\eff -> runRope (run eff) r) :& r)
{-# INLINE entwine #-}

-- -- | Change the first effect strand of the 'Rope'
-- replaceStrand :: (strand1 ~> LooseRope (strand2 ': rest) core)
--               -> LooseRope (strand1 ': rest) (LooseRope (strand2 ': rest) core) a b
--               -> LooseRope (strand2 ': rest) core a b
-- replaceStrand = undefined

-- | Runs a 'Rope' with no strands inside its core strand
untwine :: LooseRope '[] core a b -> core a b
untwine (Rope f) = f RNil
{-# INLINE untwine #-}

-- | The 'Strand' for IO effects
type IOStrand = '("io", Kleisli IO)

-- | Untwines a 'Rope' which has only an IO effect left
untwineIO :: LooseRope '[IOStrand] (Kleisli IO) a b -> a -> IO b
untwineIO (Rope f) = runKleisli $ f (Weaver id :& RNil)
{-# INLINE untwineIO #-}

-- | Lifts an IO effect in the 'IOStrand'
ioStrand :: (rope `Entwines` '[IOStrand]) => (a -> IO b) -> a `rope` b
ioStrand = strand #io . Kleisli
{-# INLINE ioStrand #-}

-- | Lifts an IO effect with no input in the 'IOStrand'
ioStrand_ :: (rope `Entwines` '[IOStrand]) => IO b -> () `rope` b
ioStrand_ = strand #io . Kleisli . const
{-# INLINE ioStrand_ #-}
