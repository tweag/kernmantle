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
import Data.CAS.ContentStore
import qualified Data.CAS.RemoteCache as Remote
import Data.Functor.Compose
import Data.Profunctor.Cayley
import Options.Applicative
import Data.Char (toUpper)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Path


data a `GetOpt` b where
  GetOpt :: String  -- ^ Name
         -> String  -- ^ Docstring
         -> Maybe String  -- ^ Default value
         -> GetOpt a String  -- ^ Returns final value

data a `FileAccess` b where
  ReadFile :: String  -- ^ File name
           -> FileAccess a BS.ByteString
  WriteFile :: String  -- ^ File name
            -> FileAccess BS.ByteString ()

data Cached a b where
  CachedOp  :: Cacher a b -> (a ~~> b) -> Cached a b

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
  liftKleisliIO id -< putStrLn $
    "I wrote the summary file. I won't do it a second time if you re-execute with the same parameters"

pipeline :: () ~~> ()
pipeline = proc () -> do
  name     <- getOpt "name" "The user's name" $ Just "Yves" -< ()
  lastname <- getOpt "lastname" "The user's last name" $ Just "Pares" -< ()
  age      <- getOpt "age" "The user's age" Nothing -< ()
    -- This demonstrates early failure: if age isn't given, this pipeline won't
    -- even start
  cnt <- strand #files (ReadFile "user") -< ()
  strand #cached (CachedOp (defaultCacherWithIdent 1) computation) -< (name, lastname, age, cnt)
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
  mapKleisli (cacheKleisliIO (Just 1) cacher Remote.NoCache store) $
    runRope $ loosen f

main :: IO ()
main =
  withStore [absdir|/home/yves/_store|] $ \store -> do
    let Cayley interpretedPipeline =
          pipeline & loosen
               & entwine  #cached  (interpretCached store)
               & entwine_ #options interpretGetOpt
               & entwine_ #files   interpretFileAccess
               & untwine
    Kleisli runPipeline <- execParser $ info (helper <*> interpretedPipeline) $
         header "A kernmantle pipeline with caching"
      <> progDesc "Doesn't do _that_ much more than exCli"
    runPipeline ()
