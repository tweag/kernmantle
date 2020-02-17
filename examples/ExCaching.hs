{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}


-- | In this example, we show how to make an effect that can expose options it
-- wants, then accumulate these options into a regular Parser (from
-- optparse-applicative) which builds a pipeline.

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Arrow
import Control.Category
import Control.Monad.IO.Class
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as Remote
import Data.Functor.Compose
import Data.Profunctor.Cayley
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Path


-- | An effect to ask for some parameter's value
data a `GetOpt` b where
  GetOpt :: String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe String  -- ^ Default value
         -> GetOpt a String  -- ^ Returns final value

-- | An effect to read or write a file
data a `FileAccess` b where
  ReadFile :: String  -- ^ File name
           -> FileAccess a BS.ByteString
  WriteFile :: String  -- ^ File name
            -> FileAccess BS.ByteString ()

-- | An effect to cache part of the pipeline
data Cached a b where
  CachedOp  :: CS.Cacher a b -> (a ~~> b) -> Cached a b

-- | The pipeline type that we will use. It collects some effects we need. Note
-- this is actually a polymorphic type, it requires the listed effects, but
-- could accept more, and the order doesn't matter.
type a ~~> b = forall m. (MonadIO m) =>
  AnyRopeWith '[ '("options", GetOpt)
               , '("files", FileAccess)
               , '("cached", Cached) ]
              '[Arrow, HasKleisli m] a b


-- | Some dummy computation that will be cached
computation :: (String, String, String, BS.ByteString) ~~> ()
computation = proc (name, lastname, age, cnt) -> do
  let summary = "Your name is " ++ name ++ " " ++ lastname ++
                " and you are " ++ age ++ ".\n"
  liftKleisliIO id -< putStrLn $ "Gonna write this summary: "<>summary
  strand #files (WriteFile "summary") -< BS8.pack summary <> cnt
  printStuff <- strand #options
    (GetOpt "print-stuff" "Whether to print stuff after writing the summary" $ Just "yes") -< ()
  -- This option is retrieved from inside the cached computation, so it won't be
  -- taken into account when determining whether it should be reexecuted
  liftKleisliIO id -< if head printStuff == 'y'
    then putStrLn $
      "I wrote the summary file. I won't do it a second time if you re-execute with the same parameters"
    else return ()

-- | The full pipeline of effects to execute
pipeline :: () ~~> ()
pipeline = proc () -> do
  name     <- getOpt "name" "The user's name" $ Just "Yves" -< ()
  lastname <- getOpt "lastname" "The user's last name" $ Just "Pares" -< ()
  age      <- getOpt "age" "The user's age" Nothing -< ()
    -- This demonstrates early failure: if age isn't given, this pipeline won't
    -- even start as the cli Parser will fail
  cnt <- strand #files (ReadFile "user") -< ()
  strand #cached (CachedOp (CS.defaultCacherWithIdent 1) computation)
      -< (name, lastname, age, cnt)
  where
    getOpt n d v = strand #options $ GetOpt n d v
    
-- | The core effect we need to collect all our options and build the
-- corresponding CLI Parser. What we should really run once our strands of
-- effects have been evaluated. It can be pretty general, as long as in the
-- bottom it allows us to access some IO monad. Note that @CoreEff a b = Parser
-- (a -> IO b)@ if eff is just @Kleisli IO@
type CoreEff a b = forall eff m. (HasMonadIO eff m) => Cayley Parser eff a b

-- | Turns a GetOpt into an actual optparse-applicative Parser
interpretGetOpt :: GetOpt a b -> CoreEff a b
interpretGetOpt (GetOpt name docstring defVal) =
  Cayley $ liftKleisliIO . const . return <$>
  let (docSuffix, defValField) = case defVal of
        Just v -> (". Default: "<>v, value v)
        Nothing -> ("", mempty)
  in strOption ( long name <> help (docstring<>docSuffix) <>
                 metavar (map toUpper name) <> defValField )

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

interpretCached store runRope (CachedOp cacher f) =
  mapKleisli (CS.cacheKleisliIO (Just 1) cacher Remote.NoCache store) $
    runRope $ loosen f

main :: IO ()
main =
  CS.withStore [absdir|/home/yves/_store|] $ \store -> do
    let Cayley pipelineParser =
          pipeline & loosen
              -- Order matters here: since interpretCached needs to run the full
              -- rope (as each 'Cached' computation must be able to access all
              -- the other effects), its entwine call must be the first:
            & entwine  #cached  (interpretCached store)
            & entwine_ #options interpretGetOpt
            & entwine_ #files   interpretFileAccess
            & untwine
    Kleisli runPipeline <- execParser $ info (helper <*> pipelineParser) $
         header "A kernmantle pipeline with caching"
      <> progDesc "Doesn't do _that_ much more than exCli"
    runPipeline ()
