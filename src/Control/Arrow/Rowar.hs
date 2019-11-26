{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Arrow.Rowar
  ( Product(..)
  , Tannen(..)
  )
where

import Control.Category
import Control.Arrow

import Data.Bifunctor.Tannen
import Data.Bifunctor.Product
import Data.Typeable
import Data.Vinyl
import GHC.Exts
import GHC.TypeLits

import Prelude hiding (id, (.))


-- data Altern f g a b where
--   AlternId :: AlternId f g a a
--   Altern   :: f a x -> g x y -> Altern f g y b -> Altern f g a b

-- runAltern :: (Arrow ar)
--           => (forall x y. f x y -> ar x y)
--           -> (forall x y. g x y -> ar x y)
--           -> Altern f g a b
--           -> ar a b
-- runAltern _ _ AlternId = arr id
-- runAltern runF runG (Altern f g r) =
--   runF f >>> runG g >>> runAltern runF runG r

-- instance Category (ASeq f g) where
--   id = AlternId
--   a . AlternId = a
--   a . Altern f g a' = Altern f g (a . a')  -- Problematic: quadratic complexity

-- instance (Arrow f, Arrow g) => Arrow (Altern f g) where
--   arr f = Altern (arr f) id AlternId
--   first (Altern f g r) = Altern (first f) (first g) (first r)

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
newtype StrandRunner (ar::Strand) (eff::(Symbol,Strand)) = StrandRunner
  { runStrand :: Snd eff ~> ar }

-- | Constructs a free arrow out of several * -> * -> * effects ('Strand's) that
-- can be interlaced
newtype Twine_ (record::TwineRec) (effs::Strands) (cnst::Strand -> Constraint) a b =
  Twine
  { runTwine ::
      forall ar. (cnst ar) => record (StrandRunner ar) effs -> ar a b }
type Twine = Twine_ ARec
type LooseTwine = Twine_ Rec

instance Category (Twine_ r s Category) where
  id = Twine $ const id
  Twine f . Twine g = Twine $ \r -> f r . g r

-- DUPLICATE (TODO: Find a better way)
instance Category (Twine_ r s Arrow) where
  id = Twine $ const id
  Twine f . Twine g = Twine $ \r -> f r . g r

-- DUPLICATE
instance Category (Twine_ r s ArrowChoice) where
  id = Twine $ const id
  Twine f . Twine g = Twine $ \r -> f r . g r

instance Arrow (Twine_ r s Arrow) where
  arr f = Twine $ const $ arr f
  first (Twine f) = Twine $ first . f
  second (Twine f) = Twine $ second . f
  Twine f *** Twine g = Twine $ \r -> f r *** g r

-- DUPLICATE
instance Arrow (Twine_ r s ArrowChoice) where
  arr f = Twine $ const $ arr f
  first (Twine f) = Twine $ first . f
  second (Twine f) = Twine $ second . f
  Twine f *** Twine g = Twine $ \r -> f r *** g r

instance ArrowChoice (Twine_ r s ArrowChoice) where
  left (Twine f) = Twine $ left . f
  right (Twine f) = Twine $ right . f
  Twine f ||| Twine g = Twine $ \r -> f r ||| g r

tightenTwine :: LooseTwine s c a b -> Twine s c a b
tightenTwine = undefined

loosenTwine :: Twine s c a b -> LooseTwine s c a b
loosenTwine = undefined
