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
-- A 'Strand' is an effet with a parameter and an output. No constraint are
-- placed on the 'Strand', but once combined in a 'Rope', that 'Rope' will be
-- an 'Arrow' and a 'Profunctor'. 'Strand's in a 'Rope' are named via labels to
-- limit ambiguity.
--
-- An action that targets some 'Strand' can be lifted to a 'Rope' that contains
-- that 'Strand' with the 'strand' function.

module Control.Kernmantle.Rope
  ( Rope
  , TightRope, LooseRope
  , BinEff, Strand
  , StrandName, StrandEff
  , InRope(..)
  , AnyRopeWith
  , Entwines, SatisfiesAll
  , Label, fromLabel
  , Sieve(..), ToSieve
  , EffBuilder, UnaryBuilder
  , WrappingEff
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

  , withEffWrapper, getEffWrapper
  , onEachEffFunctor
  , entwineEffFunctors
  )
where

import Control.Category
import Control.Arrow
import Data.Bifunctor
import Data.Biapplicative
import Data.Bifunctor.Tannen
import Data.Function ((&))
import Data.Profunctor hiding (rmap)
import Data.Profunctor.Sieve
import Data.Vinyl hiding ((<+>))
import Data.Vinyl.ARec
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import GHC.Exts
import GHC.OverloadedLabels

import Prelude hiding (id, (.))

import Control.Kernmantle.Error
import Control.Kernmantle.Functors
import Control.Kernmantle.Rope.Internal


-- | 'Rope' is a free arrow built out of _several_ binary effects (ie. effects
-- with kind * -> * -> *). These effects are called 'Strand's, they compose the
-- @mantle@, can be interpreted in an @interp@ effect and can be interlaced "on
-- top" of an existing @core@ effect.
newtype Rope record mantle core a b = Rope { getRopeRunner :: RopeRunner record mantle core core a b }
  deriving ( Bifunctor, Biapplicative
           , Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus
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
  strand :: Label l -> eff :-> rope

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
        => LooseRope m core :-> TightRope m core
tighten r = mkRope $ runRope r . fromARec
{-# INLINE tighten #-}

-- | Turn a 'TightRope' into a 'LooseRope'
loosen :: (NatToInt (RLength m))
       => TightRope m core :-> LooseRope m core
loosen r = mkRope $ runRope r . toARec
{-# INLINE loosen #-}

-- | Adds a new effect strand in the 'Rope'. Users of that function should
-- normally not place constraints on the core or instanciate it. Rather,
-- requirement of the execution function should be expressed in terms of other
-- effects of the @mantle@.
--
-- @(a :-> b)@ is the type function from an effect @a@ to an effect @b@
-- preserving the input/output type params of these effects. It allows us to
-- elude the two type parameters of these effects, but these are still here.
entwine :: Label name  -- ^ Give a name to the strand
        -> (binEff :-> LooseRope mantle core) -- ^ The execution function
        -> LooseRope ('(name,binEff) ': mantle) core -- ^ The 'Rope' with an extra effect strand
       :-> LooseRope mantle core -- ^ The 'Rope' with the extra effect strand
                                     -- woven in the core
entwine _ run rope = mkRope $ \r ->
  runRope rope (Weaver (\eff -> runRope (run eff) r) :& r)
{-# INLINE entwine #-}

-- | Runs an effect directly in the core. You should use that function only as
-- part of a call to 'entwine'.
asCore :: core :-> Rope r mantle core
asCore = mkRope . const
{-# INLINE asCore #-}

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

-- | Strips a 'Rope' of its empty mantel.
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
     -- ^ The rope to split
 :-> LooseRope rest core
onEachEffFunctor runWrapper runMantle1 runMantle2 =
  runMantle1 . entwineEffFunctors runWrapper (untwine . runMantle2)
{-# INLINE onEachEffFunctor #-}

