{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Control.Kernmantle.Caching
  ( CS.ContentStore
  , CS.ContentHashable (..)
  , ProvidesCaching (..)
  , ProvidesPosCaching (..)
  , AutoIdent (..)
  , SomeHashable (..)
  , StoreWithId (..)
  , CachingContext
  , CS.withStore
  , caching, caching'
  ) where

import Control.Category
import Control.Arrow
import Control.Kernmantle.Arrow
import Control.Kernmantle.Rope
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Data.CAS.ContentHashable as CS
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as Remote
import Data.Functor.Identity (Identity)
import Data.Store (Store)

import Prelude hiding (id, (.))


instance CS.ContentHashable Identity SplitId
instance CS.ContentHashable Identity ArrowIdent


data SomeHashable where
  SomeHashable ::
    (CS.ContentHashable Identity a, Show a) => a -> SomeHashable
instance CS.ContentHashable Identity SomeHashable where
  contentHashUpdate ctx (SomeHashable a) = CS.contentHashUpdate ctx a
type CachingContext = [SomeHashable]
instance Show SomeHashable where
  show (SomeHashable a) = show a


-- | A class to cache part of the pipeline
class ProvidesCaching eff where
  usingStore :: (CS.ContentHashable Identity a, Store b)
            => eff a b
            -> eff a b
-- | A class to cache part of the pipeline where the hash can depend on the
-- position of the task in the pipeline
class (ProvidesCaching eff) => ProvidesPosCaching eff where
  usingStore' :: (CS.ContentHashable Identity a, Store b)
             => eff a b
             -> eff a b

instance {-# OVERLAPPABLE #-} (Functor f, ProvidesCaching eff)
  => ProvidesCaching (f ~> eff) where
  usingStore (Cayley f) = Cayley $ fmap usingStore f
instance {-# OVERLAPPABLE #-} (Functor f, ProvidesPosCaching eff)
  => ProvidesPosCaching (f ~> eff) where
  usingStore' (Cayley f) = Cayley $ fmap usingStore' f

-- | Bundles together a identifier for the whole pipeline. If identifier is
-- Nothing, no caching will be performed.
data StoreWithId = StoreWithId CS.ContentStore (Maybe Int)

instance (MonadIO m, MonadBaseControl IO m, MonadMask m)
  => ProvidesCaching (Reader StoreWithId ~> Kleisli m) where
  usingStore =
    mapReader $ \(StoreWithId store pipelineId) ->
    mapKleisli $ \act input ->
      CS.cacheKleisliIO
       pipelineId (CS.defaultCacherWithIdent 1) Remote.NoCache store
       act input

instance (Arrow eff, ProvidesCaching eff)
  => ProvidesCaching (Writer CachingContext ~> eff) where
  usingStore =
    mapWriter_ $ \newContext eff ->
      arr (,newContext) >>> usingStore (eff . arr fst)
      -- New context is just added as phantom input to the underlying effect
instance (Arrow eff, ProvidesPosCaching eff) => ProvidesPosCaching (Writer CachingContext ~> eff) where
  usingStore' =
    mapWriter_ $ \newContext eff ->
      arr (,newContext) >>> usingStore' (eff . arr fst)

instance (ProvidesCaching eff) => ProvidesCaching (AutoIdent eff) where
  usingStore (AutoIdent f) = AutoIdent $ usingStore . f
instance (Arrow eff, ProvidesCaching eff) => ProvidesPosCaching (AutoIdent eff) where
  usingStore' (AutoIdent f) = AutoIdent $ \aid ->
    arr (,aid) >>> usingStore (f aid . arr fst)

instance (ProvidesCaching core) => ProvidesCaching (Rope r m core) where
  usingStore r = mkRope $ \w -> usingStore (runRope r w)
instance (ProvidesPosCaching core) => ProvidesPosCaching (Rope r m core) where
  usingStore' r = mkRope $ \w -> usingStore' (runRope r w)

-- | Any rope whose core provides caching can run cached tasks. The task is
-- identified by its position in the pipeline
caching' :: (ProvidesPosCaching core, CS.ContentHashable Identity a, Show a, Store b)
         => Rope r mantle core a b -> Rope r mantle core a b
caching' = mapRopeCore usingStore'

-- | Any rope whose core provides caching can run cached tasks. The task is
-- identified by an explicit identifier
caching :: (Arrow core, ProvidesCaching core
           ,CS.ContentHashable Identity ident, CS.ContentHashable Identity a
           ,Store b)
        => ident -> Rope r mantle core a b -> Rope r mantle core a b
caching n r = arr (,n) >>> mapRopeCore usingStore (r . arr fst)
