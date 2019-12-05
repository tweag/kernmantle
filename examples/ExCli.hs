{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | In this example, we show how to condition some strand based on a _builder_
-- effect (here just accessing some config value) that will create our strands.

import Control.Kernmantle.Builder
import Control.Kernmantle.Error
import Control.Kernmantle.Rope
import Control.Arrow
import Data.Functor.Compose
import System.Environment (getArgs)


-- | The Console effect
data a `Console` b where
  GetLine :: () `Console` String
  PutLine :: String `Console` ()

-- | Run a Console effect in IO
runConsole :: a `Console` b -> a -> IO b
runConsole cmd inp = case cmd of
  GetLine -> getLine
  PutLine -> putStrLn $ ">> "++ inp

-- | Like a Console but which can only write or do nothing
data a `Logger` b where
  Log :: String `Logger` ()
  NoLog :: String `Logger` ()

runLogger :: a `Logger` b -> a -> IO b
runLogger Log = putStrLn
runLogger NoLog = const $ return ()

-- | The File effect
data a `File` b where
  GetFile :: FilePath `File` String
  PutFile :: (FilePath, String) `File` ()

-- | Run a File effect in IO
runFile :: a `File` b -> a -> IO b
runFile cmd inp = case cmd of
  GetFile -> readFile inp
  PutFile -> uncurry writeFile inp

-- | A verbosity level (for our logger effect)
data VerbLevel = Silent | Error | Warning | Info
  deriving (Eq, Ord, Bounded, Show, Read)

-- | A builder effect to deactivate some effects depending on some verbosity
-- level. This is the simplest effect possible that we use as a builder here: a
-- pure function (->).
--
-- This builder input is a some 'VerbLevel'. Its output will some some effect
-- depending on it
type WithVerbControl = EffBuilder VerbLevel (->)

-- | Here we added our builder effect on top of the #logger effect that will
-- control the verbosity. This appears in the type of the #logger effect
type a ~~> b =
  AnyRopeWith '[ '("console", Console)
               , '("logger", WithVerbControl Logger)
               , '("file", File) ]
              '[ArrowChoice, TryEffect IOException]
              a b

-- | Controls the verbosity level before logging in a 'Console' effect
logS :: VerbLevel   -- ^ Minimal verbosity
     -> AnyRopeWith '[ '("logger", WithVerbControl Logger) ] '[]
                    String ()
logS minVerb = strand #logger $
  effbuild $ -- We indicate that this strand will need to access an effect
             -- builder ('effpure' just lifts an effect)
    \level -> -- our builder is just a pure function, taking a verbosity level
              -- as an input
      if level >= minVerb then Log else NoLog
      -- then we just have to return the Logger effect based on the verbosity
      -- level it received as input.

getContentsToOutput :: FilePath ~~> String
getContentsToOutput = proc filename -> do
  if filename == ""  -- This branching requires ArrowChoice. That's why we put
                     -- it in our type alias as a condition
    then strand #console GetLine <<< logS Info -< "Reading from stdin"
    else do
      res <- tryE (strand #file GetFile) -< filename
      case res of
        Left e -> do
          logS Error -< "Had an error:\n" ++ displayException (e::IOError)
          returnA -< "..."
        Right str -> do
          logS Info -< "I read from " ++ filename ++ ":\n" ++ str
          returnA -< str

-- | The Arrow program we will want to run
prog :: String ~~> ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  strand #console PutLine -< contents

getVerbLevel :: IO VerbLevel
getVerbLevel = do
  [vl] <- getArgs
  return $! read vl

-- | main details every strand, but we can skip the #io strand and the merging
-- strand an directly interpret every strand in the core effect
main :: IO ()
main = do
  vl <- getVerbLevel
  prog & loosen
       & onEachEffFunctor -- Every EffBuilder is an instance of EffFunctor
         (\f -> runEffBuilder f vl)
             -- First, how to run the builder effect, which is a pure function,
             -- by giving it the verbosity level read from CLI
         ( entwine #logger (asCore . toSieve . runLogger) )
             -- Then, how to run the effects that were wrapped in this builder
             -- effect
         ( entwine #console (asCore . toSieve . runConsole)
         . entwine #file (asCore . toSieve . runFile) )
             -- And finally, how to run all the other effects that were _not_
             -- wrapped
       & runSieveCore "You"
