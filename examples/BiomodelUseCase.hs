{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Profunctor.Cayley
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Typeable (Typeable, typeOf)
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

import qualified Graphics.Vega.VegaLite as VL


type ODESystem = Double -> [Double] -> [Double]
  -- ^ The RHS of the system, taking absolute time in input (useful if our
  -- function contains time-dependent parameters).  Invariant: if @f ::
  -- ODESystem@, then in @y = f t x@, @x@ and @y@ should always have the same
  -- length

type InitConds = [Double]
  -- ^ Initial conditions. Should have the same length than the inputs of the
  -- 'ODESystem'

data ODEModel = ODEModel { odeSystem :: ODESystem
                         , odeVarNames :: [T.Text]
                         , odeInitConds :: InitConds }

-- | An effect to simulate an ODE system
data SimulateODE a b where
  SimulateODE :: SimulateODE ODEModel (L.Matrix Double)
    -- ^ Given a model of N variables to solve, simulates it and returns a
    -- matrix with N+1 columns, first column being the time steps

-- | An effect to ask for some parameter's value
data GetOpt a b where
  -- | Get a raw string option
  GetOpt :: (Show b, Typeable b) -- So we can show the default value and its
                                 -- type
         => [String] -- ^ Namespace
         -> ReadM b  -- ^ How to parse the option
         -> String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe b  -- ^ Default value
         -> GetOpt a b  -- ^ Returns final value
  -- | Ask to select between alternatives
  Switch :: [String]  -- ^ Namespace
         -> (String, String, b) -- ^ Name and docstring of default
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
  Log :: [String] -- ^ Namespace
      -> Logger String ()

-- | An class to cache part of the pipeline
class WithCaching eff where
  cache :: CS.Cacher a b -> eff a b -> eff a b

-- | Any core that has caching provides it to the rope:
instance (WithCaching core) => WithCaching (Rope r m core) where
  cache = mapRopeCore . cache

-- | For options parsed by IsString class
getStrOpt n d v = strand #options $ GetOpt [] str n d v

-- | For options parsed by Read class
getOpt n d v = strand #options $ GetOpt [] auto n d v

-- | An Applicative version of 'getOpt'
getOptA n d v = ArrowMonad $ getOpt n d v

-- | Unwrap an applicative to get back an arrow
appToArrow (ArrowMonad a) = a

genViz :: AnyRopeWith
  '[ '("options", GetOpt) ]
  '[Arrow]
   (ODEModel, L.Matrix Double) VL.VegaLite
genViz = proc (model, mtx) -> do
  let
    timeCol:varCols = L.toColumns mtx
    dat = VL.dataFromColumns []
      . VL.dataColumn "Time" (VL.Numbers $ L.toList timeCol)
      . foldr (.) id
        (map (\(cn,ys) -> VL.dataColumn cn (VL.Numbers $ L.toList ys)) $
             zip (odeVarNames model) varCols)
    trans = VL.transform
      . VL.foldAs (odeVarNames model) "Variable" "Value"
        -- Creates two new columns for each column, so we can use them for
        -- Y-position and color encoding
    enc = VL.encoding
      . VL.position VL.X [ VL.PName "Time", VL.PmType VL.Quantitative ]
      . VL.position VL.Y [ VL.PName "Value", VL.PmType VL.Quantitative ]
      . VL.color         [ VL.MName "Variable", VL.MmType VL.Nominal ]
  (w,h) <- getOpt "viz-size" "(width,height) of the visualisation" (Just (800,800)) -< ()
  returnA -< VL.toVegaLite [ trans [], dat [], enc [], VL.mark VL.Line []
                           , VL.height h, VL.width w ]

data SolTimes = SolTimes { solStart      :: Double
                         , solEnd        :: Double
                         , solTimepoints :: Int }

expandSolTimes :: SolTimes -> L.Vector Double
expandSolTimes st =
  L.linspace (solTimepoints st) (solStart st, solEnd st)

-- | Solve an ODE model in any Arrow Rope that has a GetOpt effect
interpretSimulateODE :: SimulateODE a b
                     -> AnyRopeWith '[ '("options", GetOpt), '("logger",Logger) ] '[Arrow] a b
interpretSimulateODE SimulateODE = proc mdl -> do
  -- We ask for some parameters related to the simulation process:
  times <- appToArrow $ SolTimes
    <$> getOptA "start" "T0 of simulation" (Just 0)
    <*> getOptA "end" "Tmax of simulation" (Just 50)
    <*> getOptA "timepoints" "Num timepoints of simulation" (Just 1000) -< ()
  -- We ask for the solving algorithm to use:
  solvingFn <- strand #options $ Switch []
    ("cv", "Solve with CVode algorithm. See https://computing.llnl.gov/projects/sundials/cvode", CV.odeSolve)
    [("ark", "Solve with ARKode algorithm. See https://computing.llnl.gov/projects/sundials/arkode", ARK.odeSolve)]
    -< ()
  -- Finally we can solve the system:
  let resMtx = solvingFn (odeSystem mdl) (odeInitConds mdl) (expandSolTimes times)
  -- We concat the time vector as the first column of the result matrix before
  -- returning it:
  strand #logger (Log []) -< "Done solving"
  returnA -< L.asColumn (expandSolTimes times) L.||| resMtx
  
-- | Provides some Rope a strand to solve models. This Rope can perform anything
-- it wants in terms of computations and IO, as it is also given files and
-- options accesses. BUT if it asks for files and options, @pipelineForModel@
-- takes care of disambiguating the option names and file paths, in case several
-- models are solved in the same whole pipeline.
withODEStrand
  :: (AllInMantle '[ '("options", GetOpt), '("files", FileAccess), '("logger", Logger) ] mantle core
     ,Arrow core)
  => String  -- ^ Model name
  -> TightRope ( '("ode", SimulateODE) ': mantle ) core a b
             -- ^ The rope needing an "ode" strand of effects to solve ODE models
  -> TightRope mantle core a b
             -- ^ The rope with no more "ode" strand
withODEStrand modelName =
  mapStrand #options changeGetOpt .
  mapStrand #files   changeFileAccess .
  mapStrand #logger  changeLogger .
  tighten . entwine #ode (.interpretSimulateODE) . loosen
  where
    changeGetOpt :: GetOpt a b -> GetOpt a b
    changeGetOpt (GetOpt ns p n ds v) = GetOpt (modelName:ns) p n ds v
    changeGetOpt (Switch ns def alts) = Switch (modelName:ns) def alts

    changeFileAccess :: FileAccess a b -> FileAccess a b
    changeFileAccess (WriteFile f e) = WriteFile (modelName<>"-"<>f) e
    changeFileAccess (ReadFile f e)  = ReadFile  (modelName<>"-"<>f) e

    changeLogger :: Logger a b -> Logger a b
    changeLogger (Log ns) = Log (("Model_"<>modelName):ns)

-- | Run a Console effect in any effect that can access a Kleisli m where m is a
-- MonadIO
interpretLogger :: (HasKleisliIO m eff) => Logger a b -> eff a b
interpretLogger (Log ns) = liftKleisliIO $
  putStrLn . (intercalate "." ns <> ">> "++)

-- | Weave a FileAccess into any rope that can access options (to possibly
-- rebind the default file path), a logger (to log when a result has been
-- written), and a MonadIO (to performs the access). Doesn't support accessing
-- twice the same file for now.
interpretFileAccess
  :: (MonadIO m)
  => FileAccess a b
  -> AnyRopeWith '[ '("options",GetOpt), '("logger",Logger) ] '[Arrow, HasKleisli m] a b
interpretFileAccess (ReadFile name ext) =
  getStrOpt ("read-"<>name) ("File to read "<>name<>" from") (Just $ name<>"."<>ext)
  >>> liftKleisliIO BS.readFile
interpretFileAccess (WriteFile name ext) = proc content -> do
  realPath <- getStrOpt ("write-"<>name) ("File to write "<>name<>" to") (Just (name<>"."<>ext)) -< ()
  liftKleisliIO id -< LBS.writeFile realPath content
  strand #logger (Log []) -< "Wrote file "<>realPath

addNS [] n = n
addNS ns n = intercalate "-" ns <> "-" <> n

-- | Turns a GetOpt into an actual optparse-applicative Parser
interpretGetOpt :: (Arrow eff) => GetOpt a b -> Cayley Parser eff a b
interpretGetOpt (GetOpt ns parser name docstring defVal) =
  Cayley $ arr . const <$>
  let (docSuffix, defValField) = case defVal of
        Just v -> (". Default: "<>show v, value v)
        Nothing -> ("", mempty)
  in option parser ( long (addNS ns name) <> help (docstring<>docSuffix) <>
                     metavar (show $ typeOf $ fromJust defVal) <> defValField )
interpretGetOpt (Switch ns (defName,defDocstring,defVal) alts) =
  Cayley $ arr . const <$> foldr (<|>) defFlag (map toFlag' alts)
  where
    defFlag = flag defVal defVal (long (addNS ns defName) <> help defDocstring)
    toFlag' (name, docstring, val) = flag' val (long (addNS ns name) <> help docstring)


--- * THE PIPELINE

-- | The vanderpol model
vanderpol :: Double -> ODEModel
vanderpol mu =
  ODEModel (\_t [x,v] -> [v, -x + mu * v * (1-x*x)])
           ["x","v"]
           [1,0]

-- | Solve a specific model. To do so, requires an "ode" strand.
solveVanderpol :: AnyRopeWith
  '[ '("ode", SimulateODE), '("files", FileAccess), '("options", GetOpt) ]
  '[Arrow] () (L.Matrix Double)
solveVanderpol = proc () -> do
  odeModel <- appToArrow $
    vanderpol <$> getOptA "mu" "µ parameter" (Just 2) -< ()
  res <- strand #ode $ SimulateODE -< odeModel
  strand #files $ WriteFile "res" "csv" -< mkHeader (odeVarNames odeModel) <> encodeMatrix res
  viz <- genViz -< (odeModel, res)
  strand #files $ WriteFile "viz" "html" -< LTE.encodeUtf8 $ VL.toHtml $ viz
  returnA -< res
  where
    mkHeader vars = LTE.encodeUtf8 $ LT.fromStrict $
      "Time," <> T.intercalate "," vars <> "\r\n"
      -- SimulateODE result first column will always be Time, other columns are
      -- model's variables

-- | The final pipeline to run
pipeline = withODEStrand "vanderpol" solveVanderpol

--- * END OF PIPELINE


-- | The core effect we need to collect all our options and build the
-- corresponding CLI Parser, and hold the store for caching.
type CoreEff = Cayley Parser (Kleisli (ReaderT CS.ContentStore IO))

instance WithCaching CoreEff where
  cache c = mapKleisli $ \act i -> do
    store <- ask
    CS.cacheKleisliIO (Just 1) c Remote.NoCache store act i

main :: IO ()
main = do
  let Cayley pipelineParser =
          pipeline & loosen
            & entwine  #files   (.interpretFileAccess)
            & entwine_ #logger  interpretLogger
            & entwine_ #options interpretGetOpt
            & untwine
  Kleisli runPipeline <- execParser $ info (helper <*> pipelineParser) $
         header "A kernmantle pipeline solving a biomodel"
  CS.withStore [absdir|/home/yves/_store|] $ runReaderT $ runPipeline ()
  return ()
