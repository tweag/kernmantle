{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | In this example, we show how to condition some strand based on
-- ahead-of-time effects (here CLI parsing) which just need to instanciate
-- Functor

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

-- | An AoT effect to deactivate some effects depending on some verbosity
-- level. This is a Functor.
type VerbControl = (->) VerbLevel

-- | Here we added an AoT effect on top of the console effect that will control
-- the verbosity
type a ~~> b =
  AnyRopeWith '[ '("console", Console)
               , '("logger", UnaryBuilder VerbControl Logger)
               , '("file", File) ]
              '[ArrowChoice, TryEffect IOException]
              a b

data VerbLevel = Silent | Error | Warning | Info
  deriving (Eq, Ord, Bounded, Show, Read)

-- | Controls the verbosity level before logging in a 'Console' effect
logS :: VerbLevel   -- ^ Minimal verbosity
     -> AnyRopeWith '[ '("logger", UnaryBuilder VerbControl Logger) ] '[]
                    String ()
logS minVerb = strand #logger $ unaryBuilder $ \level ->
  if level >= minVerb then Log else NoLog

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
       & onEachEffFunctor
         (\f -> unaryFromBuilder f vl)
             -- First, how to run the config effect, which is a pure function,
             -- by giving it the verbosity level read from CLI
         ( entwine #logger (asCore . toSieve . runLogger) )
             -- Then, how to run the effects that were wrapped in these effect
             -- wrappers
         ( entwine #console (asCore . toSieve . runConsole)
         . entwine #file (asCore . toSieve . runFile) )
             -- And finally, how to run all the other effects that were _not_
             -- wrapped
       & runSieveCore "You"
