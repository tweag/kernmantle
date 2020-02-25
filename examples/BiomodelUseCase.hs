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

-- | This example solves an ODE model (vanderpol for now), exposing its
-- parameters to the outside world. Left to do is just cache the solving, plot
-- the results and show that we can solve several models inside the same
-- pipeline.

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Arrow
import Control.Category
import Control.DeepSeq (($!!))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as Remote
import Data.Csv.HMatrix
import Data.Functor.Compose
import Data.Profunctor.Cayley
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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

-- | An effect to simulate an ODE system
data SimulateODE a b where
  SimulateODE :: SimulateODE ODEModel (L.Matrix Double)

-- | An effect to ask for some parameter's value
data GetOpt a b where
  -- | Get a raw string option
  GetOpt :: (Show b, Read b)
         => String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe b  -- ^ Default value
         -> GetOpt a b  -- ^ Returns final value
  -- | Ask to select between alternatives
  Switch :: (String, String, b) -- ^ Name and docstring of default
         -> [(String, String, b)]
            -- ^ Names and docstrings of alternatives
         -> GetOpt a b

-- | An effect to read or write a file
data FileAccess a b where
  ReadFile :: String  -- ^ File name, without extension
           -> String  -- ^ File extension
           -> FileAccess a BS.ByteString
  WriteFile :: String  -- ^ File name, without extension
            -> String  -- ^ File extension
            -> FileAccess LBS.ByteString ()

-- | The Console effect
data Logger a b where
  Log :: Logger String ()

-- | An class to cache part of the pipeline
class WithCaching eff where
  cache :: CS.Cacher a b -> eff a b -> eff a b

-- | Any core that has caching provides it to the rope:
instance (WithCaching core) => WithCaching (Rope r m core) where
  cache = mapRopeCore . cache

getOpt n d v = strand #options $ GetOpt n d v
getOptA n d v = ArrowMonad $ getOpt n d v
appToArrow (ArrowMonad a) = a

data LinspaceSolTimes = LinspaceSolTimes { solStart      :: Double
                                         , solEnd        :: Double
                                         , solTimepoints :: Int }

vanderpol :: Double -> LinspaceSolTimes -> ODEModel
vanderpol mu st =
  ODEModel (\_t [x,v] -> [v, -x + mu * v * (1-x*x)])
           [1,0]
           (L.linspace (solTimepoints st) (solStart st, solEnd st))

-- | Solve a specific model. To do so, requires an "ode" strand.
solveVanderpol :: AnyRopeWith
  '[ '("ode", SimulateODE), '("files", FileAccess), '("options", GetOpt) ]
  '[Arrow] () ()
solveVanderpol = proc () -> do
  odeModel <- appToArrow $
    vanderpol <$> getOptA "mu" "µ parameter" (Just 2)
              <*> (LinspaceSolTimes
                    <$> getOptA "start" "T0 of simulation" (Just 0)
                    <*> getOptA "end" "Tmax of simulation" (Just 50)
                    <*> getOptA "timepoints" "Num timepoints of simulation" (Just 1000)) -< ()
  res <- strand #ode $ SimulateODE -< odeModel
  strand #files $ WriteFile "res" "csv" -< encodeMatrix res  


-- | Solve an ODE model in any Rope that has a GetOpt effect
interpretSimulateODE :: SimulateODE a b
                     -> AnyRopeWith '[ '("options", GetOpt) ] '[Arrow] a b
interpretSimulateODE SimulateODE = proc (ODEModel sys initConds times) -> do
  solvingFn <- strand #options $
    Switch  ("cv", "Solve with CV", CV.odeSolve)
           [("ark", "Solve with Advanced Range-Kutta", ARK.odeSolve)] -< ()
  returnA -< solvingFn sys initConds times
  
-- | Provides some Rope a strand to solve models. This Rope can perform anything
-- it wants in terms of computations and IO, as it is also given files and
-- options accesses. BUT if it asks for files and options, @pipelineForModel@
-- takes care of disambiguating the option names and file paths, in case several
-- models are solved in the same whole pipeline.
withODEStrand
  :: (AllInMantle '[ '("options", GetOpt), '("files", FileAccess) ] mantle core
     ,Arrow core)
  => String  -- ^ Model name
  -> TightRope ( '("ode", SimulateODE) ': mantle ) core a b
             -- ^ The rope needing an "ode" strand of effects to solve ODE models
  -> TightRope mantle core a b
             -- ^ The rope with no more "ode" strand
withODEStrand modelName =
  mapStrand #options changeGetOpt .
  mapStrand #files   changeFileAccess .
  tighten . entwine #ode (.interpretSimulateODE) . loosen
  where
    changeGetOpt :: GetOpt a b -> GetOpt a b
    changeGetOpt (GetOpt n ds v) =
      GetOpt (modelName<>"-"<>n) ("Model "<>modelName<>": "<>ds) v
    changeGetOpt (Switch def alts) = Switch (go def) (map go alts)
      where go (n,ds,v) = (modelName<>"-"<>n, "Model "<>modelName<>": "<>ds, v)

    changeFileAccess :: FileAccess a b -> FileAccess a b
    changeFileAccess (WriteFile f e) = WriteFile (modelName<>"-"<>f) e
    changeFileAccess (ReadFile f e)  = ReadFile  (modelName<>"-"<>f) e


-- | The final pipeline to run
pipeline = withODEStrand "vanderpol" solveVanderpol

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
        Just v -> let v' = show v in (". Default: "<>v', value v')
        Nothing -> ("", mempty)
  in read <$> (strOption ( long name <> help (docstring<>docSuffix) <>
                 metavar (map toUpper name) <> defValField ))
interpretGetOpt (Switch (defName,defDocstring,defVal) alts) =
  Cayley $ liftKleisliIO . const . return <$> foldr (<|>) defFlag (map toFlag' alts)
  where
    defFlag = flag defVal defVal (long defName <> help defDocstring)
    toFlag' (name, docstring, val) = flag' val (long name <> help docstring)

-- | Run a Console effect in any effect that can access a Kleisli m where m is a
-- MonadIO
interpretLogger :: (HasKleisliIO m eff) => Logger a b -> eff a b
interpretLogger Log = liftKleisliIO $ putStrLn . (">> "++)

-- | An option string to get a filepath
fpParser :: String -> String -> String -> Parser String
fpParser fname ext prefix = strOption
  ( long (prefix<>fname) <> help ("File bound to "<>fname<>". Default: "<>def)
    <> metavar "PATH" <> value def )
  where def = fname<>"."<>ext

-- | Turns a FileAccess into an option that requests a real filepath, and
-- performs the access (doesn't support accessing twice the same file for now)
interpretFileAccess :: FileAccess a b -> CoreEff a b
interpretFileAccess (ReadFile name ext) = Cayley $ f <$> fpParser name ext "read-"
  where f realPath = liftKleisliIO $ const $ BS.readFile realPath
interpretFileAccess (WriteFile name ext) = Cayley $ f <$> fpParser name ext "write-"
  where f realPath = liftKleisliIO $ LBS.writeFile realPath

main :: IO ()
main = do
    let Cayley pipelineParser =
          pipeline & loosen
            & entwine_ #files   interpretFileAccess
            & entwine_ #logger  interpretLogger
            & entwine_ #options interpretGetOpt
            & untwine
    Kleisli runPipeline <- execParser $ info (helper <*> pipelineParser) $
         header "A kernmantle pipeline solving a biomodel"
    CS.withStore [absdir|/home/yves/_store|] $ runReaderT $ runPipeline ()
