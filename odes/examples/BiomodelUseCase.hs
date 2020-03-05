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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

-- | This example:
--
-- - creates an 'ODESolving' type
-- - describes a vanderpol model as an 'ODESolving'
-- - describes a simple chemical equation as a 'ChemicalModel'
-- - builds a pipeline exposing to the user the parameters of these models
--   and of the solver
-- - converts the chemical model to an 'ODESolving'
-- - solves the models, caching the results
-- - writes the results as CSV files (paths are exposed via CLI options)
-- - generates vega lite visualisations of the results
--
-- More generally this examples shows how to use kernmantle effects to
-- build all these features.
--
-- All the pipeline primitives present in this file are generic,
-- they could be reused as-is for different 'ODESolving's. See 'pipeline'.

import Prelude hiding (id, (.))

import Control.Kernmantle.Arrow
import Control.Kernmantle.Caching
import Control.Kernmantle.Rope
import Control.Arrow
import Control.Category
import Control.DeepSeq (deepseq)
import Control.Monad.IO.Class
import Data.Csv.HMatrix
import Data.Functor.Identity (Identity)
import Data.Functor.Compose
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Profunctor
import Data.Profunctor.Cayley
import Data.Profunctor.SieveTrans
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Typeable (Typeable, typeOf)
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M

import qualified Numeric.Sundials.ARKode.ODE as ARK
import qualified Numeric.Sundials.CVode.ODE  as CV
import qualified Numeric.LinearAlgebra as L
import           Numeric.Sundials.Types

import qualified Graphics.Vega.VegaLite as VL

import Path
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

--------------------------------------------------------------------------------
-- ODE models

type ODESystem = Double -> [Double] -> [Double]
  -- ^ The RHS of the system, taking absolute time in input (useful if our
  -- function contains time-dependent parameters).  Invariant: if @f ::
  -- ODESystem@, then in @y = f t x@, @x@ and @y@ should always have the same
  -- length

type InitConds = [Double]
  -- ^ Initial conditions. Should have the same length than the inputs of the
  -- 'ODESystem'

-- | Our first effect: an ODE model to solve
data ODESolving params result = ODESolving
  { odeSystem :: params -> ODESystem  -- ^ Given the model params, build an ODESystem
  , odePostProcess :: L.Vector Double -> L.Matrix Double -> result
      -- ^ Given the time steps used and the solving results, compute the final
      -- result
  , odeVarNames :: [T.Text]  -- ^ Names of the variables (and of the columns of the matrix)
  , odeInitConds :: InitConds -- ^ Initial values of the variables
  }
instance Profunctor ODESolving where
  dimap f g mdl = mdl{odeSystem = \p -> odeSystem mdl (f p)
                     ,odePostProcess = \t m -> g (odePostProcess mdl t m)}

--------------------------------------------------------------------------------
-- Chemical models

-- | The name of the chemical element
type Element = String

-- | A dynamical system determined by a chemical reaction.
data ChemicalModel = ChemicalModel
  { -- | The left-hand side of the chemical reaction.
    chemicalLeft      :: M.Map Element Int
    -- | The right-hand side of the chemical reaction.
  , chemicalRight     :: M.Map Element Int
    -- | The initial quantities (in moles) of each element
  , chemicalInitConds :: M.Map Element Double
  } deriving (Show)

-- | Change the direction of arrows for the chemical reaction system.
reverseChemicalModel :: ChemicalModel -> ChemicalModel
reverseChemicalModel cm@(ChemicalModel {chemicalLeft=l, chemicalRight=r}) =
  cm {chemicalLeft=r, chemicalRight=l}

-- | Given reaction velocities in each sense, convert a chemical system into a
-- differential equation system for the quantity of each element.
--
-- The model parameters are the reaction rates (from left to right and from
-- right to left)
chemicalToODESolving :: ChemicalModel
                     -> ODESolving (Double,Double) (L.Vector Double, L.Matrix Double)
chemicalToODESolving chemMdl =
  ODESolving { odeSystem = odeSystem
             , odePostProcess = (,)
             , odeVarNames = map T.pack vars
             , odeInitConds = ics }
  where
    (vars, ics) = unzip $ M.toList $ M.intersectionWith (\_ ic -> ic)
      (M.union (chemicalLeft chemMdl) (chemicalRight chemMdl))
      (chemicalInitConds chemMdl)
    
    odeSystem (lr,rl) _t xs =
      zipWith combine (go chemMdl xs)
                      (go (reverseChemicalModel chemMdl) xs)
      where
        combine x y = lr * x + rl * y
    
    system m xs = product (M.intersectionWith term is qs)
      where
        keys = M.keys m
        is = fmap fst m
            -- this assumes that variables are in aphabetical order
        qs = M.fromAscList $ zipWith (,) keys xs
        fact n = fromIntegral $ product [1..n]
        term i q = q ** fromIntegral i / fromIntegral (fact i)

    go (ChemicalModel{chemicalLeft=l, chemicalRight=r}) xs = fmap (* system m xs) v
      where
        m :: M.Map Element (Int,Int)
        m =
          M.unionWith
          (\(x1,y1) (x2,y2) -> (x1 + x2, y1 + y2))
          (fmap (\x -> (x,0)) l)
          (fmap (\y -> (0,y)) r)

        v :: [Double]
        v = M.elems $ fmap (\(i,o) -> fromIntegral (o - i)) m

--------------------------------------------------------------------------------
-- Effect definition

-- | An effect to ask for some parameter's value
data GetOpt a b where
  -- | Get a raw string option
  GetOpt :: (Show b, Typeable b, ContentHashable Identity b)
             -- So we can show the default value and its type, and add the
             -- selected value to the cache context
         => ReadM b  -- ^ How to parse the option
         -> String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe b  -- ^ Default value
         -> GetOpt a b  -- ^ Returns final value
  -- | Ask to select between alternatives
  Switch :: (Show b, ContentHashable Identity b)
         => (String, String, b) -- ^ Name and docstring of default
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

-- | Used by option parser and logger, to know where the option and line to log
-- comes from
type Namespace = [String]

-- | If an effect can hold a namespace.
class Namespaced eff where
  -- | Add an element to the namespace
  addToNamespace :: String -> eff a b -> eff a b

-- | How to add a namespace to tasks in a Rope. Several effects require to be
-- interpreted in some @Cayley (Reader Namespace)@ because they need a namespace
-- to construct their effects
instance Namespaced (Reader Namespace ~> eff) where
  addToNamespace n eff = reading $ \ns ->
                           runReader (ns++[n]) eff

-- | Any rope whose core has namespacing has namespacing too
instance (Namespaced core) => Namespaced (Rope r m core) where
  addToNamespace = mapRopeCore . addToNamespace

-- | For options parsed by IsString class
getStrOpt n d v = strand #options $ GetOpt str n d v

-- | For options parsed by Read class
getOpt n d v = strand #options $ GetOpt auto n d v

-- | An Applicative version of 'getOpt'
getOptA n d v = ArrowMonad $ getOpt n d v

-- | Unwrap an applicative to get back an arrow
appToArrow (ArrowMonad a) = a

-- | Generate a vega-lite visualisation of the results of a simulation
genViz :: AnyRopeWith
  '[ '("options", GetOpt) ]
  '[Arrow]
   (L.Vector Double, [T.Text], L.Matrix Double) VL.VegaLite
genViz = proc (timeVec, colNames, mtx) -> do
  let
    varCols = L.toColumns mtx
    dat = VL.dataFromColumns []
      . VL.dataColumn "Time" (VL.Numbers $ L.toList timeVec)
      . foldr (.) id
        (map (\(cn,ys) -> VL.dataColumn cn (VL.Numbers $ L.toList ys)) $
             zip colNames varCols)
    trans = VL.transform
      . VL.foldAs colNames "Variable" "Value"
        -- Creates two new columns for each variable of the model, so we can use
        -- the variable name and value resp. for color encoding and Y-position
        -- encoding
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
  deriving (Generic)

data ODESolvingAlgorithm = CVode | ARKode
  deriving (Show, Generic)
instance (Monad m) => ContentHashable m ODESolvingAlgorithm

solveWith CVode = CV.odeSolve
solveWith ARKode = ARK.odeSolve

expandSolTimes :: SolTimes -> L.Vector Double
expandSolTimes st =
  L.linspace (solTimepoints st) (solStart st, solEnd st)

--------------------------------------------------------------------------------
-- Effect interpretation

-- | Solve an ODE model in any Arrow Rope that can cache computations, get
-- options, do logging and write files. Writes the results to csv files and
-- their vega-lite visualizations to html files (the folder to write in will be
-- determined by the current namespace).
interpretODESolving
  :: Bool  -- ^ Whether to write results
  -> ODESolving a b
  -> AnyRopeWith '[ '("options", GetOpt), '("logger",Logger), '("files",FileAccess) ]
                 '[Arrow] a b
interpretODESolving writeResults odeSolving = proc params -> do
  -- We ask for an override of the initial conditions:
  ics <- getOpt "ics" ("Initial conditions for variables "++show (odeVarNames odeSolving))
         (Just $ odeInitConds odeSolving) -< ()
  -- We ask for some parameters related to the simulation process:
  times <- appToArrow $ SolTimes
    <$> getOptA "start" "T0 of simulation" (Just 0)
    <*> getOptA "end" "Tmax of simulation" (Just 50)
    <*> getOptA "timepoints" "Num timepoints of simulation" (Just 1000) -< ()
  -- We ask for the solving algorithm to use:
  solvingAlg <- strand #options $ Switch
    ("cv", "Solve with CVode algorithm. See https://computing.llnl.gov/projects/sundials/cvode", CVode)
    [("ark", "Solve with ARKode algorithm. See https://computing.llnl.gov/projects/sundials/arkode", ARKode)]
    -< ()
  -- Finally we can solve the system:
  strand #logger Log -< "Start solving"
  let timeVec = expandSolTimes times
      resMtx = solveWith solvingAlg (odeSystem odeSolving params) ics timeVec
  (if writeResults then writeRes else arr (const ())) -< (timeVec, resMtx)
  returnA -< odePostProcess odeSolving timeVec resMtx
  where
    writeRes = proc (timeVec, resMtx) -> do
      strand #logger Log -< resMtx `deepseq` "Done solving"
      strand #files $ WriteFile "res" "csv" -<
        mkHeader (odeVarNames odeSolving) <> encodeMatrix (L.asColumn timeVec L.||| resMtx)
        -- We write the time vector as first column in the CSV
      viz <- genViz -< (timeVec, odeVarNames odeSolving, resMtx)
      strand #files $ WriteFile "viz" "html" -< LTE.encodeUtf8 $ VL.toHtml $ viz
    mkHeader vars = LTE.encodeUtf8 $ LT.fromStrict $
      "Time," <> T.intercalate "," vars <> "\r\n"

-- | Builds a namespace prefix, with some beginning, separator and ending. If
-- namespace is empty, returns the empty string.
nsPrefix :: Namespace -> String -> String -> String -> String
nsPrefix [] _ _ _ = ""
nsPrefix ns beg sep end = beg <> intercalate sep ns <> end

-- | Run a Console effect in any effect that can access a namespace and an
-- AutoIdent
interpretLogger :: (HasAutoIdent eff' eff, HasKleisli IO eff')
                => Logger a b -> (Reader Namespace ~> eff) a b
interpretLogger Log =
  reading $ \ns ->  -- We need the namespace as for each line logged, we
                            -- want to print in which namespace it was logged
    liftAutoIdent $ \ai ->
      let pref = nsPrefix ns "" "." ""<>"["<>show ai<>"]>> "
      in liftKleisli $ putStrLn . (pref <>)

-- | Weave a FileAccess into any rope with our CoreEff that can access options
-- (to give the option to rebind the default file path), a logger (to log when a
-- result has been written), and a MonadIO (to performs the access).
--
-- Needs the namespace to know the default paths to which we should write/read
-- the files. When writing, creates the directory tree if some are missing.
interpretFileAccess
  :: Namespace  -- ^ Will be used as folders
  -> FileAccess a b
  -> AnyRopeWith '[ '("options",GetOpt), '("logger",Logger) ] '[Arrow, HasKleisli IO] a b
interpretFileAccess ns (ReadFile name ext) =
  getStrOpt ("read-"<>name) ("File to read "<>name<>" from")
                            (Just $ nsPrefix ns "" "/" "/" <> name<>"."<>ext)
  >>>
  liftKleisli BS.readFile
interpretFileAccess ns (WriteFile name ext) = proc content -> do
  realPath <- getStrOpt ("write-"<>name) ("File to write "<>name<>" to")
                                         (Just $ nsPrefix ns "" "/" "/" <> name<>"."<>ext) -< ()
  liftKleisli id -< do
    createDirectoryIfMissing True $ takeDirectory realPath
    LBS.writeFile realPath content
  strand #logger Log -< "Wrote file "<>realPath

-- | Performs an action in a functor, adds this action result to
-- the caching context, and returns the result in the underlying effect
addingToCachingContext
  :: (ContentHashable Identity b, Show b, Functor f, Arrow eff)
  => f b  -- ^ Action to get the value to add
  -> (f ~> Writer CachingContext ~> eff) a b
addingToCachingContext getValue =
  fmapping getValue $ \finalValue ->
    writing [SomeHashable finalValue] $
      returning finalValue

-- | Given a Namespace to generate CLI flag names, turns a GetOpt into an actual
-- optparse-applicative Parser, and adds the value of the option given by the
-- user to the CachingContext
--
-- interpretGetOpt is the only weaver that needs access to basically every layer
-- of CoreEf.f
interpretGetOpt :: (Arrow eff)
                => GetOpt a b
                -> (Reader Namespace ~> Parser ~> Writer CachingContext ~> eff) a b
interpretGetOpt (GetOpt parser name docstring defVal) =
  reading $ \ns ->
    addingToCachingContext $
      option parser (   long (nsPrefix ns "" "-" "-" <> name)
                     <> help (nsPrefix ns "In " "." ": "<>docstring<>docSuffix)
                     <> metavar (show $ typeOf $ fromJust defVal) <> defValField )
  where
    (docSuffix, defValField) = case defVal of
      Just v -> (". Default: "<>show v, value v)
      Nothing -> ("", mempty)
interpretGetOpt (Switch (defName,defDocstring,defVal) alts) =
  reading $ \ns ->
    addingToCachingContext $
      let defFlag =
            flag defVal defVal (   long (nsPrefix ns "" "-" "-" <> defName)
                                <> help (nsPrefix ns "In " "." ": "<> defDocstring) )
          toFlag' (name, docstring, val) =
            flag' val (   long (nsPrefix ns "" "-" "-" <> name)
                       <> help (nsPrefix ns "In " "." ": "<>docstring) )
      in foldr (<|>) defFlag (map toFlag' alts)

--------------------------------------------------------------------------------
-- Pipeline definition

-- | The vanderpol 'ODESolving'
vanderpol :: ODESolving Double ()
vanderpol =
  ODESolving (\mu _t [x,v] -> [v, -x + mu * v * (1-x*x)])
             (\_ _ -> ())  -- We don't want any post-processing
             ["x","v"]
             [1,0]

-- | A simple chemical equation, which we turn into an 'ODESolving'
chemical :: ODESolving Double ()
chemical =  -- Input param is the reaction rate and we don't want post-processing
  dimap (\k -> (k, k/2)) (const ()) $
    chemicalToODESolving $
      ChemicalModel (M.fromList [("H20",1)])
                    (M.fromList [("H+",2), ("O2-",1)])
                    (M.fromList [("H20",1), ("H+",0), ("O2-",0)])

-- | The final pipeline to run. It solves two models to show that the same
-- tooling can be used to solve 2 models in the same pipeline with no risks of
-- options or files conflicting (thanks to namespaces)
pipeline =
  log "Beginning pipeline"
  >>>
  addToNamespace "vdp1" vdp
  >>>
  addToNamespace "vdp2" vdp
    -- We use twice the vdp task, so we must put them in different namespaces,
    -- else their option flags and output files will conflict
  >>> 
  addToNamespace "chem"
    (getOpt "k" "Left-to-right reaction rate" (Just 2)
     >>>
     caching' (strand #ode chemical))
     -- Any option, any change as to where to write simulation results will
     -- invalidate the cache, and make the content of the 'caching' block to be
     -- re-executed. 'Nothing' here means this task is not named, so caching
     -- will identify it with an AutoIdent (determined from its position in the
     -- pipeline)
  >>>
  log "Pipeline finished"
  where
    log s = arr (const s) >>> strand #logger Log
    vdp = getOpt "mu" "µ parameter" (Just 2)
          >>>
          caching ("vanderpol"::String) (strand #ode vanderpol)
          -- This task has an identifier for caching purposes, therefore it will
          -- not use auto-ident. This means that if its 2 uses in the pipeline
          -- are parameterized exactly the same, only the first one will be
          -- executed (beware that by default their output files are different,
          -- as they aren't in the same namespace!)

--- * END OF PIPELINE

--------------------------------------------------------------------------------
-- Pipeline interpretation

-- | The core effect we need.
--
-- This type is completely equivalent to:
-- CoreEff a b =
--   Namespace -> Parser (CachingContext, ContentStore -> ArrowIdent -> a -> IO b)
type CoreEff =
     Reader Namespace -- Get the namespace we are in
  ~> Parser -- Accumulate all the wanted options and get them from CLI. The CLI
            -- flags' names are conditioned on the current namespace
  ~> Writer CachingContext -- Accumulate the context needed to know what to take
                           -- into account to perform caching
  ~> AutoIdent   -- Get an identifier for the task
     (   Reader ContentStore -- Get the content store, to cache computation at
                            -- runtime
      ~> Kleisli IO) -- This is the runtime layer, the one the pipeline executes in

main :: IO ()
main = do
  let interpretFileAccess' toCore fileAccessEffect =
        -- We need to get the namespace to invoke 'interpretFileAccess'
        reading $ \ns ->
            runReader ns $ toCore $ interpretFileAccess ns fileAccessEffect
      parserLayer =
          pipeline & loosen
            -- Interpret mantle:
            & weave  #ode     (.interpretODESolving True)
               -- interpretODESolving needs #logger and #options, so it must be
               -- weaved before them
            & weave  #files   interpretFileAccess'
               -- interpretFileAccess needs #logger and #options, so it must be
               -- weaved before them
            & weave' #logger  interpretLogger
            & weave' #options interpretGetOpt
            & untwine
            -- Execute core:
            & runReader [] -- Remove the namespace layer
            & runCayley    -- Get to the CLI parser
  storeLayer <- snd . runWriter <$> -- Remove the CachingContext layer
    -- parserLayer needs IO to be executed:
    execParser (info (helper <*> parserLayer) $
      header "A kernmantle pipeline solving chemical models")
  withStore [absdir|/tmp/_store|] $ \store -> do
    -- Once we have the store, we can execute the rest of the layers:
    storeLayer & runAutoIdent -- Remove the AutoIdent layer
               & runReader (StoreWithId store (Just 1)) -- Remove the ContentStore layer
               & perform ()    -- Finally, run the IO
  return ()
