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
  , LocalStoreWithId
  , CachingContext
  , CS.withStore
  , caching, caching'
  , localStoreWithId
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


instance (Monad m) => CS.ContentHashable m SplitId
instance (Monad m) => CS.ContentHashable m ArrowIdent


data SomeHashable where
  SomeHashable ::
    (CS.ContentHashable IO a, Show a) => a -> SomeHashable
instance CS.ContentHashable IO SomeHashable where
  contentHashUpdate ctx (SomeHashable a) = CS.contentHashUpdate ctx a
type CachingContext = [SomeHashable]
instance Show SomeHashable where
  show (SomeHashable a) = show a


-- | A class to cache part of the pipeline
class ProvidesCaching eff where
  usingStore :: (CS.ContentHashable IO a, Store b)
             => eff a b
             -> eff a b
-- | A class to cache part of the pipeline where the hash can depend on the
-- position of the task in the pipeline
class (ProvidesCaching eff) => ProvidesPosCaching eff where
  usingStore' :: (CS.ContentHashable IO a, Store b)
              => eff a b
              -> eff a b

instance {-# OVERLAPPABLE #-} (Functor f, ProvidesCaching eff)
  => ProvidesCaching (f ~> eff) where
  usingStore (Cayley f) = Cayley $ fmap usingStore f
instance {-# OVERLAPPABLE #-} (Functor f, ProvidesPosCaching eff)
  => ProvidesPosCaching (f ~> eff) where
  usingStore' (Cayley f) = Cayley $ fmap usingStore' f

-- | Bundles together a store with an identifier for the whole pipeline. If
-- identifier is Nothing, no caching will be performed.
data StoreWithId remoteCacher = StoreWithId CS.ContentStore remoteCacher (Maybe Int)

type LocalStoreWithId = StoreWithId Remote.NoCache

-- | A 'StoreWithId' with no remote caching
localStoreWithId :: CS.ContentStore -> Maybe Int -> LocalStoreWithId
localStoreWithId store ident = StoreWithId store Remote.NoCache ident

instance (MonadIO m, MonadBaseControl IO m, MonadMask m, Remote.Cacher m remoteCacher)
  => ProvidesCaching (Reader (StoreWithId remoteCacher) ~> Kleisli m) where
  usingStore =
    mapReader_ $ \(StoreWithId store remoteCacher pipelineId) ->
    mapKleisli $ \act input ->
      CS.cacheKleisliIO
       pipelineId (CS.defaultIOCacherWithIdent 1) store remoteCacher
       act input

instance (Arrow eff, ProvidesCaching eff)
  => ProvidesCaching (Writer CachingContext ~> eff) where
  usingStore =
    mapWriter_ $ \newContext eff ->
      arr (,newContext) >>> usingStore (eff . arr fst)
      -- New context is just added as phantom input to the underlying effect
instance (Arrow eff, ProvidesPosCaching eff)
  => ProvidesPosCaching (Writer CachingContext ~> eff) where
  usingStore' =
    mapWriter_ $ \newContext eff ->
      arr (,newContext) >>> usingStore' (eff . arr fst)

instance (ProvidesCaching eff) => ProvidesCaching (AutoIdent eff) where
  usingStore (AutoIdent f) = AutoIdent $ usingStore . f
instance (Arrow eff, ProvidesCaching eff) => ProvidesPosCaching (AutoIdent eff) where
  usingStore' (AutoIdent f) = AutoIdent $ \aid ->
    arr (,aid) >>> usingStore (f aid . arr fst)

instance (ProvidesCaching core) => ProvidesCaching (Rope r m core) where
  usingStore = mapRopeCore usingStore
instance (ProvidesPosCaching core) => ProvidesPosCaching (Rope r m core) where
  usingStore' = mapRopeCore usingStore'

-- | Any rope whose core provides caching can run cached tasks. The task is
-- identified by its position in the pipeline
caching' :: (ProvidesPosCaching core, CS.ContentHashable IO a, Show a, Store b)
         => Rope r mantle core a b -> Rope r mantle core a b
caching' = mapRopeCore usingStore'

-- | Any rope whose core provides caching can run cached tasks. The task is
-- identified by an explicit identifier
caching :: (Arrow core, ProvidesCaching core
           ,CS.ContentHashable IO ident, CS.ContentHashable IO a
           ,Store b)
        => ident -> Rope r mantle core a b -> Rope r mantle core a b
caching n r = arr (,n) >>> mapRopeCore usingStore (r . arr fst)
