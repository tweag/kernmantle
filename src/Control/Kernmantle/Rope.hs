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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Rope' connects together various effect 'Strand's that get interlaced
-- together.
--
-- A 'Strand' is an effet with a parameter and an output. No constraint is
-- placed on the 'Strand', but once combined in a 'Rope', that 'Rope' will be
-- an 'Arrow' and a 'Profunctor'. 'Strand's in a 'Rope' are named via labels to
-- limit ambiguity.
--
-- An action that targets some 'Strand' can be lifted to a 'Rope' that contains
-- that 'Strand' with the 'strand' function.

module Control.Kernmantle.Rope
  ( -- * Rope
    Rope
  , TightRope
  , LooseRope
  , tighten
  , loosen
  , mapRopeCore
  , mergeStrands
    -- * Binary Effects and Strands
  , BinEff
  , Strand
  , StrandName
  , StrandEff
  , InRope
  , AllInMantle
  , strand
  , mapStrand
  , liftKleisli
  , liftKleisliIO
  , mapKleisli
  , runSieveCore
    -- * Rope Interpretation
  , entwine
  , entwine_
  , retwine
  , untwine
  , onEachEffFunctor
  , entwineEffFunctors
    -- * Predicates
  , AnyRopeWith
  , TightRopeWith
  , LooseRopeWith
  , Entwines
  , SatisfiesAll
  , SieveTrans (..)
  , HasKleisli
  , HasKleisliIO
    -- * Reexports
  , type (:->)
  , Label, fromLabel
  , (&)
  )
where

import Control.Category
import Control.Arrow
import Control.Lens (over)
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifunctor.Tannen
import Data.Function ((&))
import Data.Profunctor hiding (rmap)
import Data.Profunctor.Sieve
import Data.Profunctor.SieveTrans
import Data.Profunctor.Traversing
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.ARec
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.Exts
import GHC.OverloadedLabels

import Prelude hiding (id, (.))

import Control.Kernmantle.Arrow
import Control.Kernmantle.Error
import Control.Kernmantle.Builder
import Control.Kernmantle.Rope.Internal


-- | 'Rope' is a free arrow built out of _several_ binary effects (ie. effects
-- with kind * -> * -> *). These effects are called 'Strand's, they compose the
-- @mantle@, can be interpreted in an @interp@ effect and can be interlaced "on
-- top" of an existing @core@ effect.
newtype Rope (record::RopeRec) (mantle::[Strand]) (core::BinEff) a b =
  Rope { getRopeRunner :: RopeRunner record mantle core core a b }

  deriving ( Category
           , Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
           , Profunctor, Strong, Choice, Closed, Costrong, Cochoice
           , Mapping, Traversing
           , ThrowEffect ex, TryEffect ex
           , SieveTrans sieve, HasAutoIdent eff
           , Bifunctor, Biapplicative
           )

runRope :: Rope record mantle core a b -> record (Weaver core) mantle -> core a b
runRope (Rope (RopeRunner f)) = f
{-# INLINE runRope #-}

mkRope :: (record (Weaver core) mantle -> core a b) -> Rope record mantle core a b
mkRope = Rope . RopeRunner
{-# INLINE mkRope #-}

-- | Applies a function on the core action of the Rope
mapRopeCore :: (core a b -> core a' b') -> Rope r m core a b -> Rope r m core a' b'
mapRopeCore f rope = mkRope $ f . runRope rope
{-# INLINE mapRopeCore #-}

-- | A 'Rope' that is "tight", meaning you cannot 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(1)@ on 'TightRope's whatever the number of
-- 'Strand's.
type TightRope = Rope ARec

-- | A 'Rope' that is "loose", meaning you can 'entwine' new 'Strand's to
-- it. The 'strand' function is @O(n)@ on 'LooseRope's, @n@ being the number of
-- 'Strand's.
type LooseRope = Rope Rec

-- | Tells that a strand of effects '(l,eff) is present in the rope
type family InRope l eff rope where
  InRope l eff (Rope record mantle core) =
    ( HasField record l mantle mantle eff eff
    , RecElemFCtx record (Weaver core) )

strand :: (InRope l eff (Rope r m c))
       => Label l -> eff :-> Rope r m c
strand l eff = mkRope $ \r -> weaveStrand (rgetf l r) eff
{-# INLINE strand #-}

mapStrand :: (InRope l eff (Rope r m c))
          => Label l -> (eff :-> eff) -> Rope r m c :-> Rope r m c
mapStrand l f rope =
  mkRope $ runRope rope . over (rlensfL l) (\(Weaver w) -> Weaver $ w . f)
{-# INLINE mapStrand #-}

-- | Tells whether a collection of @strands@ is in a 'Rope'.
type family rope `Entwines` (strands::[Strand]) :: Constraint where
  rope `Entwines` '[] = ()
  rope `Entwines` ('(name, eff) ': strands ) = ( InRope name eff rope
                                               , rope `Entwines` strands )

-- | Tells whether a type satisfies all the given constraints.
type family (x::k) `SatisfiesAll` (csts::[k -> Constraint]) :: Constraint where
  x `SatisfiesAll` '[] = ()
  x `SatisfiesAll` (c1 ': cnsts) = ( c1 x, x `SatisfiesAll` cnsts)

-- | A @AnyRopeWith strands costraints@ is a 'Rope' with contains the strands
-- in the variable @strands@ , and whose core effect obeys the constraints in
-- the @contraints@ variable.
type AnyRopeWith strands coreConstraints a b = forall mantle r core.
  (Rope r mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => Rope r mantle core a b

type TightRopeWith strands coreConstraints a b = forall mantle core.
  (TightRope mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => TightRope mantle core a b

type LooseRopeWith strands coreConstraints a b = forall mantle core.
  (LooseRope mantle core `Entwines` strands, core `SatisfiesAll` coreConstraints)
  => LooseRope mantle core a b

-- | Useful for functions that want to use tighten/loosen
type AllInMantle strands mantle core =
  ( NatToInt (RLength mantle)
  , RecApplicative mantle
  , RPureConstrained (IndexableField mantle) mantle
  , LooseRope mantle core `Entwines` strands
  , TightRope mantle core `Entwines` strands )
  -- vinyl constraints make it so we have to repeat the Entwines constraint, as
  -- we need it for both tight and loose ropes here

-- | Turn a 'LooseRope' into a 'TightRope'
tighten :: (RecApplicative m, RPureConstrained (IndexableField m) m)
        => LooseRope m core :-> TightRope m core
tighten r = mkRope $ runRope r . fromARec
{-# INLINE tighten #-}

-- | Turn a 'TightRope' into a 'LooseRope'. This is very often the first step
-- in a chain of 'entwine's.
loosen :: (NatToInt (RLength m))
       => TightRope m core :-> LooseRope m core
loosen r = mkRope $ runRope r . toARec
{-# INLINE loosen #-}

-- | Adds a new effect strand in the 'Rope' by decribing how to interpret that
-- in the core. The interpretation function is fed the fixpoint of all
-- interpretation functions, meaning the interpretation function can itself use
-- effects in the 'Rope'. For example, given an effect @Eff@, whose label is
-- "effect" being interpreted in a core effect @CoreEff@, one first defines an
-- interpretation function:
--
-- @
--   interpret ::
--     (LooseRope '("effect",Eff) ': mantle) core :-> CoreEff) ->
--      Eff a b -> CoreEff a b
--
--   interpret ::
--     (LooseRope '("effect",Eff) ': mantle) core :-> CoreEff) ->
--     (Eff :-> CoreEff)
-- @
--
-- where @p :-> q@ corresponds to @forall a b. p a b -> q a b@, and obtain an
-- interpreter for the strand:
--
-- @
--   strandInterpreter ::
--     LooseRope ('("effect",Eff) ': mantle) CoreEff :->
--     LooseRope mantle CoreEff
--   strandInterpreter = entwine #effect interpret
-- @
--
entwine
  :: forall name binEff mantle core.
     Label name
  -- ^ The name of the strand to be woven.
  -> ((LooseRope ('(name,binEff) ': mantle) core :-> core) -> (binEff :-> core))
  -- ^ The interpretation function for the effect, which can depend on the
  -- global interpretation function for the rope.
  -> (LooseRope ('(name,binEff) ': mantle) core :-> LooseRope mantle core)
  -- ^ An interpretation function for a 'Rope' with containing the given effect
  -- strand, transformed into the same Rope but with that effect woven in the
  -- core.
entwine _ interpFn rope = mkRope $ \r ->
  let runThatRope :: LooseRope ('(name,binEff) ': mantle) core :-> core
      runThatRope rope' =
        runRope rope' (Weaver (interpFn runThatRope) :& r)
  in runThatRope rope

-- | A version of 'entwine' where the strand can be directly interpreted in the
-- core with no need to trigger other effects. For example, given an effect
-- @Eff@, whose label is "effect" being interpreted in a core effect @CoreEff@,
-- one first defines an interpretation function:
--
-- @
--   interpret :: Eff a b -> CoreEff a b
--   interpret :: Eff :-> CoreEff
-- @
--
-- and obtain an interpreter for the strand:
--
-- @
--   strandInterpreter ::
--     LooseRope ('("effect",Eff) ': mantle) CoreEff :->
--     LooseRope mantle CoreEff
--   strandInterpreter = entwine_ #effect interpret
-- @
--
entwine_
  :: forall name binEff mantle core.
     Label name
  -- ^ The name of the strand to be woven.
  -> (binEff :-> core)
  -- ^ The interpretation function for the effect.
  -> (LooseRope ('(name,binEff) ': mantle) core :-> LooseRope mantle core)
  -- ^ An interpretation function for a 'Rope' with containing the given effect
  -- strand, transformed into the same Rope but with that effect woven in the
  -- core.
entwine_ lbl interpFn = entwine lbl (const interpFn)
{-# INLINE entwine_ #-}

-- | Reorders the strands to match some external context. @strands'@ can contain
-- more elements than @strands@. Note it works on both 'TightRope's and
-- 'LooseRope's
retwine :: (RetwinableAs r strands core strands')
        => Rope r strands core
       :-> Rope r strands' core
retwine r = mkRope $ runRope r . rcast
{-# INLINE retwine #-}

-- | Merge two strands that have the same effect type. Keeps the first name.
mergeStrands :: Label n1
             -> Label n2
             -> LooseRope ( '(n1,binEff) ': '(n2,binEff) ': mantle ) core
            :-> LooseRope ( '(n1,binEff) ': mantle ) core
mergeStrands _ _ rope = mkRope $ \(r@(Weaver w) :& rest) ->
  runRope rope (r :& Weaver w :& rest)
{-# INLINE mergeStrands #-}

-- | Strips a 'Rope' of its empty mantle. Usually the last step of the
-- interpretation of a 'Rope'.
untwine :: LooseRope '[] core :-> core
untwine r = runRope r RNil
{-# INLINE untwine #-}

-- * Creating and running binary effects from unary effects

-- | Runs a 'Rope' with no strands left when its core is a 'Sieve' (ie. is
-- mappable to a functorial/monadic effect)
runSieveCore :: (Sieve biEff uEff)
             => a -> LooseRope '[] biEff a b -> uEff b
runSieveCore a r = sieve (untwine r) a
{-# INLINE runSieveCore #-}

entwineEffFunctors
  :: (EffFunctor f, RMap (MapStrandEffs f mantle1))
  => (f core' :-> core)
  -> (LooseRope mantle2 core :-> core')
  -> LooseRope (MapStrandEffs f mantle1 ++ mantle2) core
 :-> LooseRope mantle1 core'
entwineEffFunctors f g (Rope rnr) = Rope $ unwrapSomeStrands f (g . Rope) rnr
{-# INLINE entwineEffFunctors #-}

-- | Separates a 'LooseRope' in two parts: one wrapped in EffFunctors
-- (@mantle1@) and one which is not (@mantle2) and runs them. This is a
-- convenience function to run several strands wrapped with the same
-- 'EffFunctor'.
--
-- Note that the function to run the effect wrappers is executed _once per
-- wrapper_ used along the 'Rope', so keep that function light.
--
-- Note: The UX for 'onEachEffFunctor' is not great. Though it is quite simple,
-- it forces either to run everything at once, or to do some trickery by
-- returning some effect in the core. We should try to improve that.
onEachEffFunctor
  :: (EffFunctor f, RMap (MapStrandEffs f mantle1))
  => (f core :-> core)  -- ^ Run the wrapper effects
  -> (LooseRope mantle1 core :-> LooseRope rest core)
     -- ^ Run the effects that were wrapped with the wrapper
  -> (LooseRope mantle2 core :-> LooseRope '[] core)
     -- ^ Run the effects that were not wrapped
  -> LooseRope (MapStrandEffs f mantle1 ++ mantle2) core
 :-> LooseRope rest core
onEachEffFunctor runWrapper runMantle1 runMantle2 =
  runMantle1 . entwineEffFunctors runWrapper (untwine . runMantle2)
{-# INLINE onEachEffFunctor #-}

-- groupBuilderStrands
--   :: ( RetwinableAs Rec (MapStrandEffs (EffBuilder i builderEff) runnerEffs)
--                         core flatMantle
--      , RetwinableAs Rec mantleRest core flatMantle )
--   -> Label groupedRopeName
--   -> LooseRope flatMantle core
--  :-> LooseRope ( '(groupedRopeName, EffBuilder i builderEff
--                                       (LooseRope runnerEffs core))
--                  ': mantleRest )
--                core
-- groupBuilder _ rope = mkRope $ \(Weaver weaveGroupedStrand :& rest) ->
--   runRope rope
