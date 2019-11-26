{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingVia #-}

module Control.Arrow.Twine
  ( Product(..)
  , Tannen(..)
  , Twine_(..)
  , Twine, LooseTwine
  , Strand, Strands, TwineRec
  , StrandRunner(..)
  , tightenTwine, loosenTwine
  , type (~>)
  )
where

import Control.Category
import Control.Arrow

import Control.Monad.Reader
import Data.Bifunctor.Tannen
import Data.Bifunctor.Product
import Data.Functor.Identity  -- Needed by the deriving via machinery
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import GHC.Exts
import GHC.TypeLits

import Prelude hiding (id, (.))


-- | The kind for all effects of arity 2
type Strand = * -> * -> *

-- | The kind for a type-level effect list
type Strands = [(Symbol, Strand)]

-- | The kind for records that will contain 'StrandRunner's
type TwineRec = ((Symbol, Strand) -> *) -> Strands -> *

type family Snd t where
  Snd '(a,b) = b

-- | A natural transformation on type constructors of two arguments.
type f ~> g = forall x y. f x y -> g x y

-- | Runs one strand (* -> * -> * effect) in another
newtype StrandRunner (sup::Strand) (strand::(Symbol,Strand)) = StrandRunner
  { runStrand :: Snd strand ~> sup }

-- | 'Twine_' is a free arrow built out of _several_ arity 2 effects
-- (ie. effects with kind * -> * -> *). These effects are called 'Strand's, and
-- they can be interlaced "on top" of an existing arrow @sup@ (for "support").
--
-- Note that if @sup@ is maintained polymorphic, then this corresponds to the
-- final encoding of the free arrow construction.
newtype Twine_ (record::TwineRec) (strands::Strands) (sup::Strand) a b =
  Twine
  { runTwine :: record (StrandRunner sup) strands -> sup a b }
  deriving (Category, Arrow, ArrowChoice, ArrowLoop, ArrowZero, ArrowPlus)
    via Reader (record (StrandRunner sup) strands) `Tannen` sup

type Twine = Twine_ ARec
type LooseTwine = Twine_ Rec

tightenTwine :: LooseTwine s sup a b -> Twine s sup a b
tightenTwine = undefined

loosenTwine :: Twine s sup a b -> LooseTwine s sup a b
loosenTwine = undefined
