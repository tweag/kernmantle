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

import Prelude hiding (id, (.), readFile, writeFile)

import Control.Kernmantle.Rope
  ( AnyRopeWith
  , HasKleisli (..)
  , HasMonadIO (..)
  , entwine
  , entwine_
  , untwine
  , loosen
  , strand
  , mapKleisli
  , liftKleisliIO
  , (&)
  )
import Control.Arrow (Arrow, Kleisli (..))
import Control.Category (Category (..))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.RemoteCache as Remote
import Data.Functor.Identity (Identity (..))
import Data.Profunctor.Cayley (Cayley (..))
import Data.Store (Store (..))
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Path (absdir)

--------------------------------------------------------------------------------
-- Effect definition

-- | An effect for parsing command-line options. Note how 'GetOpt' constructs
-- @GetOpt a String@, without taking any parameter of type @a@, meaning that it
-- ignores its input. In practice, we pass @()@ as a dummy input to the effect.
data a `GetOpt` b where
  GetOpt :: String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe String  -- ^ Default value
         -> GetOpt a String  -- ^ Returns final value

-- | An effect for accessing the file-system. Now we have two constructors, one
-- for reading and another for writing. The @WriteFile@ effect produces no
-- outputs, as shown by the type of 'FileAccess' it constructs.
data a `FileAccess` b where
  ReadFile :: String  -- ^ File name
           -> FileAccess a BS.ByteString
  WriteFile :: String  -- ^ File name
            -> FileAccess BS.ByteString ()

-- | An effect for optionally adding a cache to operations. The option to
-- whether to add or not a cache is given by the 'Cacher' argument.
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

--------------------------------------------------------------------------------
-- Pipeline definition

-- | Our example pipeline. As shown by its type, it neither consumes not
-- produces any value.
pipeline :: () ~~> ()
pipeline = proc () -> do
  -- Read the options using the @GetOpt@ effect. The "age" parameter also
  -- demonstrates early failure: if age isn't given, this pipeline won't even
  -- start.
  let getOpt n d v = strand #options (GetOpt n d v)
  name     <- getOpt "name" "The user's name" (Just "Yves") -< ()
  lastname <- getOpt "lastname" "The user's last name" (Just "Pares") -< ()
  age      <- getOpt "age" "The user's age" Nothing -< ()

  -- Read the "user" file.
  cnt <- readFile "user" -< ()

  -- Run the operation with the appropriate caching.
  cachedOp 1 computation -< (name, lastname, age, cnt)

  where
    -- Notice how the label "options" matches the one defined in the @~~>@ type.
    getOpt :: String -> String -> Maybe String -> (a ~~> String)
    getOpt n d v = strand #options (GetOpt n d v)

    -- Notice how the label "files" matches the one defined in the @~~>@ type.
    readFile :: String -> (a ~~> BS.ByteString)
    readFile name = strand #files (ReadFile name)

    writeFile :: String -> (BS.ByteString ~~> ())
    writeFile name = strand #files (WriteFile name)

    -- Notice how the label "cached" matches the one defined in the @~~>@ type.
    -- The constraints over @a@ and @b@ come from the 'defaultCacherWithIdent'
    -- cacher.
    cachedOp ::
      (CH.ContentHashable Identity a, Store b) =>
      Int -> (a ~~> b) -> (a ~~> b)
    cachedOp n op = strand #cached (CachedOp cacher op)
      where
        cacher = CS.defaultCacherWithIdent n

    -- Define an operation that will be cached. This operation runs in the same
    -- context as the above pipeline.
    computation :: (String, String, String, BS.ByteString) ~~> ()
    computation = proc (name, lastname, age, cnt) -> do
      let summary = "Your name is " ++ name ++ " " ++ lastname ++
                    " and you are " ++ age ++ ".\n"
      liftKleisliIO id -< putStrLn $ "Gonna write this summary: "<>summary
      -- Since the operation runs in the same context, we still have access to
      -- the 'FileSystem' effect.
      writeFile "summary" -< BS8.pack summary <> cnt
      liftKleisliIO id -< putStrLn
        "I wrote the summary file. I won't do it a second time if \
        \you re-execute with the same parameters"

--------------------------------------------------------------------------------
-- Effect interpretation

-- | The core effect needed to collect all our options and build the
-- corresponding CLI Parser. The effect we should really run once our strands
-- of effects have been evaluated. It can be pretty general, as long as in the
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

--------------------------------------------------------------------------------
-- Pipeline interpretation

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
