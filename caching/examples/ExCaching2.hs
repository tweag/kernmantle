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

-- | An class to cache part of the pipeline
class WithCaching eff where
  cache :: CS.Cacher a b -> eff a b -> eff a b

-- | Any core that has caching provides it to the rope:
instance (WithCaching core) => WithCaching (Rope r m core) where
  cache = mapRopeCore . cache

-- | The pipeline type that we will use. It collects some effects we need. Note
-- this is actually a polymorphic type, it requires the listed effects, but
-- could accept more, and the order doesn't matter.
type a ~~> b = forall m. (MonadIO m) =>
  AnyRopeWith '[ '("options", GetOpt)
               , '("files", FileAccess) ]
              '[Arrow, HasKleisli m, WithCaching] a b

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
  cache (CS.defaultCacherWithIdent 1) computation -< (name, lastname, age, cnt)
  where
    getOpt n d v = strand #options $ GetOpt n d v
    
-- | The core effect we need to collect all our options and build the
-- corresponding CLI Parser, and hold the store for caching.
type CoreEff = Parser ~> Kleisli (ReaderT CS.ContentStore IO)

instance WithCaching CoreEff where
  cache c = mapKleisli (\act i -> do
                           store <- ask
                           CS.cacheKleisliIO (Just 1) c Remote.NoCache store act i)

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

main :: IO ()
main = do
    let Cayley pipelineParser =
          pipeline & loosen
              -- Order matters here: since interpretCached needs to run the full
              -- rope (as each 'Cached' computation must be able to access all
              -- the other effects), its weave call must be the first:
            & weave' #options interpretGetOpt
            & weave' #files   interpretFileAccess
            & untwine
    Kleisli runPipeline <- execParser $ info (helper <*> pipelineParser) $
         header "A kernmantle pipeline with caching"
      <> progDesc "Does the same than exCaching, but caching is done with a class"
    CS.withStore [absdir|/tmp/_store|] $ runReaderT $ runPipeline ()
