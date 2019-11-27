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

module Control.Kernmantle
  ( Product(..)
  , Tannen(..)
  , Rope(..)
  , TightRope, LooseRope
  , Strand, NamedStrand, RopeRec
  , Weaver(..)
  , Label, fromLabel
  , tighten, loosen
  , type (~>)
 
  , untwine
  -- , entwine
  , strand
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
import Data.Functor.Identity  -- Needed by the deriving via machinery
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import GHC.Exts
import GHC.TypeLits
import GHC.OverloadedLabels

import Prelude hiding (id, (.))


-- | The kind for all effects of arity 2
type Strand = * -> * -> *

-- | The kind for a type-level effect list
type NamedStrand = (Symbol, Strand)

type family StrandName (t::NamedStrand) where
  StrandName '(a, b) = a

type family StrandEff (t::NamedStrand) where
  StrandEff '(a, b) = b

-- | The kind for records that will contain 'Weaver's
type RopeRec = (NamedStrand -> *) -> [NamedStrand] -> *

-- | A natural transformation on type constructors of two arguments.
type f ~> g = forall x y. f x y -> g x y

-- | Weaves one "mantle" strand (* -> * -> * effect) in a "core" strand
newtype Weaver (core::Strand) (strand::NamedStrand) = Weaver
  { runStrand :: StrandEff strand ~> core }

-- | 'Rope' is a free arrow built out of _several_ arity 2 effects
-- (ie. effects with kind * -> * -> *). These effects are called 'Strand's, they
-- compose the @mantle@, and they can be interlaced "on top" of an existing
-- @core@ effect.
newtype Rope (record::RopeRec) (mantle::[NamedStrand]) (core::Strand) a b =
  Rope
    { runRope :: record (Weaver core) mantle -> core a b }
  
  deriving (Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           ,Bifunctor)
    via Reader (record (Weaver core) mantle) `Tannen` core

  deriving (Profunctor, Strong, Choice)
    via Reader (record (Weaver core) mantle) `Cayley` core

-- | A with over any core that satisfies some constraints.
--
-- NOTE: Given @core@ is maintained universally quantified, a 'Rope' created
-- this way corresponds to the final encoding of the free arrow construction.
type UniRope cst record mantle a b =
  forall core. (cst core) => Rope record mantle core a b

-- | A 'Rope' that is "tight", meaning you cannot 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(1)@ on 'TightRope's whatever the number of
-- 'Strand's.
type TightRope = Rope ARec

-- | A 'Rope' that is "loose", meaning that you can 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(n)@ on 'LooseRope's, @n@ being the number of
-- 'Strand's.
type LooseRope = Rope Rec

-- type IsInRope (record::RopeRec) (strands::[NamedStrand]) (core::Strand) (strand::NamedStrand) =
--   ( HasField record (StrandName strand) strands strands (Snd strand) (Snd strand)
--   , RecElemFCtx record (Weaver core) )

-- | Tells whether @strand@ is in a 'Rope'
type family (namedStrand::NamedStrand) `InRope` twine where
  nstrand `InRope` Rope record mantle core =
    ( HasField record (StrandName nstrand) mantle mantle (StrandEff nstrand) (StrandEff nstrand)
    , RecElemFCtx record (Weaver core) )  

tighten :: LooseRope s core a b -> TightRope s core a b
tighten = undefined

loosen :: TightRope s core a b -> LooseRope s core a b
loosen = undefined

-- -- | Adds a new effect strand to the 'Rope' mantle
-- entwine :: (forall s x y. StrandEff nstrand x y -> LooseRope rest s x y)
--         -> LooseRope (nstrand ': rest) (LooseRope rest core) a b
--         -> LooseRope rest core a b
-- entwine run (Rope f) = Rope $ \r -> f (Weaver run :& r)
-- {-# INLINE entwine #-}

-- -- | Change the first effect strand of the 'Rope'
-- replaceStrand :: (strand1 ~> LooseRope (strand2 ': rest) core)
--               -> LooseRope (strand1 ': rest) (LooseRope (strand2 ': rest) core) a b
--               -> LooseRope (strand2 ': rest) core a b
-- replaceStrand = undefined

-- | Runs a 'Rope' with no strands inside its core strand
untwine :: LooseRope '[] core a b -> core a b
untwine (Rope f) = f RNil
{-# INLINE untwine #-}

-- | Lift an effect in the 'Rope'. Performance should be better with a
-- 'TightRope' than with a 'LooseRope', unless you have very few 'Strand's.
strand :: ('(l,strand) `InRope` Rope r s core)
       => Label l -> strand a b -> Rope r s core a b 
strand l eff = Rope $ \r -> runStrand (rgetf l r) eff
{-# INLINE strand #-}
