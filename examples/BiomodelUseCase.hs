{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This example implements caching with a slightly different API than
-- ExCaching. Instead of encoding caching as an effect to be put in the mantle,
-- it encodes it as a class to be added as a constraint on the core

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Arrow
import Control.Category
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as Remote
import Data.Functor.Compose
import Data.Profunctor.Cayley
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Path

import qualified Numeric.Sundials.ARKode.ODE as ARK
import qualified Numeric.Sundials.CVode.ODE  as CV
import qualified Numeric.LinearAlgebra as L
import           Numeric.Sundials.Types


type ODESystem = Double -> [Double] -> [Double]
  -- ^ The RHS of the system, taking absolute time in input (useful if our
  -- function contains time-dependent parameters).  Invariant: if @f ::
  -- ODESystem@, then in @y = f t x@, @x@ and @y@ should always have the same
  -- length

type InitConds = [Double]
  -- ^ Initial conditions. Should have the same length than the inputs of the
  -- 'ODESystem'

type SolTimes = L.Vector Double
  -- ^ The timepoints to compute

data ODEModel = ODEModel { odeSystem :: ODESystem
                         , odeInitConds :: InitConds
                         , odeSolTimes :: SolTimes }

--data ParamModel = 

-- | An effect to simulate an ODE system
data SimulateODE a b where
  SimulateODE :: SimulateODE ODEModel (L.Matrix Double)

-- | An effect to ask for some parameter's value
data GetOpt a b where
  GetOpt :: String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe String  -- ^ Default value
         -> GetOpt a String  -- ^ Returns final value

-- | An effect to read or write a file
data FileAccess a b where
  ReadFile :: String  -- ^ File name
           -> FileAccess a BS.ByteString
  WriteFile :: String  -- ^ File name
            -> FileAccess BS.ByteString ()

-- | The Console effect
data Logger a b where
  Log :: Logger String ()

-- | An class to cache part of the pipeline
class WithCaching eff where
  cache :: CS.Cacher a b -> eff a b -> eff a b

-- | Any core that has caching provides it to the rope:
instance (WithCaching core) => WithCaching (Rope r m core) where
  cache = mapRopeCore . cache

-- | The pipeline type that we will use. It collects some effects we need. Note
-- this is actually a polymorphic type, it requires the listed effects, but
-- could accept more, and the order doesn't matter.
type a ~~> b =
  AnyRopeWith '[ '("options", GetOpt)
               , '("files", FileAccess)
               , '("ode", SimulateODE)
               , '("logger", Logger) ]
              '[Arrow, WithCaching] a b

getOpt n d v = strand #options $ GetOpt n d v


-- | The full pipeline of effects to execute
pipeline :: () ~~> ()
pipeline = proc () -> do
  cnt <- strand #files (ReadFile "user") -< ()
  cache (CS.defaultCacherWithIdent 1) id -< ()
    
-- | The core effect we need to collect all our options and build the
-- corresponding CLI Parser, and hold the store for caching.
type CoreEff = Cayley Parser (Kleisli (ReaderT CS.ContentStore IO))

instance WithCaching CoreEff where
  cache c = mapKleisli $ \act i -> do
    store <- ask
    CS.cacheKleisliIO (Just 1) c Remote.NoCache store act i

-- | Turns a GetOpt into an actual optparse-applicative Parser
interpretGetOpt :: GetOpt a b -> CoreEff a b
interpretGetOpt (GetOpt name docstring defVal) =
  Cayley $ liftKleisliIO . const . return <$>
  let (docSuffix, defValField) = case defVal of
        Just v -> (". Default: "<>v, value v)
        Nothing -> ("", mempty)
  in strOption ( long name <> help (docstring<>docSuffix) <>
                 metavar (map toUpper name) <> defValField )

-- | Run a Console effect in any effect that can access a Kleisli m where m is a
-- MonadIO
interpretLogger :: (HasKleisliIO m eff) => Logger a b -> eff a b
interpretLogger Log = liftKleisliIO $ putStrLn . (">> "++)

-- | Solve an ODE model in any arrow
interpretSimulateODE :: (Arrow eff) => SimulateODE a b -> eff a b
interpretSimulateODE SimulateODE = arr $ \(ODEModel sys initConds times) ->
  CV.odeSolve sys initConds times

-- | An option string to get a filepath
fpParser :: String -> String -> Parser String
fpParser fname prefix = strOption
  ( long (prefix<>fname) <> help ("File bound to "<>fname<>". Default: "<>def)
    <> metavar "PATH" <> value def )
  where def = fname<>".txt"

-- | Turns a FileAccess into an option that requests a real filepath, and
-- performs the access (doesn't support accessing twice the same file for now)
interpretFileAccess :: FileAccess a b -> CoreEff a b
interpretFileAccess (ReadFile name) = Cayley $ f <$> fpParser name "read-"
  where f realPath = liftKleisliIO $ const $ BS.readFile realPath
interpretFileAccess (WriteFile name) = Cayley $ f <$> fpParser name "write-"
  where f realPath = liftKleisliIO $ BS.writeFile realPath

main :: IO ()
main = do
    let Cayley pipelineParser =
          pipeline & loosen
            & entwine_ #options interpretGetOpt
            & entwine_ #files   interpretFileAccess
            & entwine_ #ode     interpretSimulateODE
            & entwine_ #logger  interpretLogger
            & untwine
    Kleisli runPipeline <- execParser $ info (helper <*> pipelineParser) $
         header "A kernmantle pipeline with caching"
      <> progDesc "Does the same than exCaching, but caching is done with a class"
    CS.withStore [absdir|/home/yves/_store|] $ runReaderT $ runPipeline ()
