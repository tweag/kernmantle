{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Kernmantle.AoTEffects where

import Data.Bifunctor.Tannen


-- | Adds some ahead-of-time (functorial) unary effects to any binary effect,
-- that should be executed /before/ we get to the binary effect. These
-- ahead-of-time effects can be CLI parsing, access to some configuration,
-- pre-processing of the compute graph, etc.
type WithAoTEff a f = Tannen f a

withAoTEff :: f (eff x y) -> (eff `WithAoTEff` f) x y
withAoTEff = Tannen
