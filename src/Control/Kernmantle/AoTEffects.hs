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
type WithAoT a f = Tannen f a

withAoT :: f (eff x y) -> (eff `WithAoT` f) x y
withAoT = Tannen
