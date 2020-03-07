{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | In this example we show how, depending on some value read at load-time, to
-- have a strand that is 'Sum' of effects, so that its main effect can be
-- bypassed without the need for 'ArrowChoice'.

import Control.Kernmantle.Error
import Control.Kernmantle.Rope
import Control.Arrow
import Data.Bifunctor.Sum
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

runLogger :: a `Logger` b -> a -> IO b
runLogger Log = putStrLn

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

-- | Wraps a 'Logger' effect to it can be bypassed based on some 'VerbLevel'
type MbLogger = Reader VerbLevel ~> Sum (->) Logger

-- | Given we wrap the #logger effect to bypass it or not depending on some
-- 'VerbLevel', this appears in its type.
type a ~~> b =
  AnyRopeWith '[ '("console", Console)
               , '("logger", MbLogger)
               , '("file", File) ]
              '[ArrowChoice, TryEffect IOException]
              a b

-- | Controls the verbosity level before logging in a 'Console' effect. This
-- logic could happen at 'weave' time, but here we show that we also can
-- implement at the 'Rope' level if it is needed, and by doing so that effects
-- can be built out of 'Cayley' too.
logS :: VerbLevel   -- ^ Minimal verbosity
     -> AnyRopeWith '[ '("logger", MbLogger) ] '[]
                    String ()
logS minVerb = strand #logger $
  reading $ \level -> -- our builder is just a pure function, taking a verbosity
                      -- level as an input
    if level >= minVerb  -- Depending on the verbosity level:
    then L2 (const ())   -- we either bypass the Log effect
    else R2 Log          -- or we perform it

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

-- | The Arrow program we want to run
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
       & weaveK #logger (\eff -> case runReader vl eff of
                            L2 f -> return . f
                            R2 eff -> runLogger eff)
          -- The #logger weaver is oblivious of the bypassing logic, it just
          -- gives the 'VerbLevel' to the effect
       & weaveK #console runConsole
       & weaveK #file runFile
       & untwine
       & perform "You"
