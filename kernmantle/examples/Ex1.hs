{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

import Control.Kernmantle.Error
import Control.Kernmantle.Rope
import Control.Arrow


-- | The Console effect
data a `Console` b where
  GetLine :: () `Console` String
  PutLine :: String `Console` ()

-- | Run a Console effect in IO
runConsole :: a `Console` b -> a -> IO b
runConsole cmd inp = case cmd of
  GetLine -> getLine
  PutLine -> putStrLn $ ">> "++ inp

-- | The File effect
data a `File` b where
  GetFile :: FilePath `File` String
  PutFile :: (FilePath, String) `File` ()

-- | Run a File effect in IO
runFile :: a `File` b -> a -> IO b
runFile cmd inp = case cmd of
  GetFile -> readFile inp
  PutFile -> uncurry writeFile inp

-- | Just to show the type of runFile once we lift it as part of the 'Rope'
interpFile :: a `File` b
           -> AnyRopeWith '[ '("io", Kleisli IO) ] '[] a b
              -- We give our list of requirements. Interpreting File effects
              -- will require an #io Strand to put the IO actions in. Kleisli is
              -- a binary effect constructed from a monadic effect, it's the
              -- prime way to lift IO actions in the Rope. No requirements are
              -- placed on the core.
interpFile = strand #io . Kleisli . runFile

-- | Just a shortcut to say our program can run in any rope that supports our
-- effects and whose core implements ArrowChoice and allows us to catch
-- IOExceptions
type a ~~> b =
  AnyRopeWith '[ '("console",Console), '("warnConsole",Console), '("file",File) ]
              '[ArrowChoice, TryEffect IOException]
              a b

getContentsToOutput :: FilePath ~~> String
getContentsToOutput = proc filename -> do
  if filename == ""  -- This branching requires ArrowChoice. That's why we put
                     -- it in our type alias as a condition
    then strand #console GetLine <<< strand #warnConsole PutLine -< "Reading from stdin"
    else do
      res <- tryE (strand #file GetFile) -< filename
      returnA -< case res of
        Left e -> "Had an error:\n" ++ displayException (e::IOError)
        Right str -> "I read from " ++ filename ++ ":\n" ++ str

-- | The Arrow program we will want to run
prog :: String ~~> ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  strand #console PutLine -< contents

main :: IO ()
main = prog -- Rope ARec '[("warnConsole",Console),("console",Console),("file",File),("io",Kleisli IO)] (Kleisli IO) String ()
            & loosen -- We tell that prog should be a TightRope, and at the same
                     -- time turn it into a LooseRope, whose effects can be
                     -- executed one after the other
            -- Rope Rec '[("warnConsole",Console),("console",Console),("file",File),("io",Kleisli IO)] (Kleisli IO) String ()
            & mergeStrands #console #warnConsole
               -- We used a second Console effect for warnings in prog. We
               -- redirect it to #console
            -- Rope Rec '[("console",Console),("file",File),("io",Kleisli IO)] (Kleisli IO) String ()
            & weave #console (\runRope consoleEff ->
                runRope $ loosen $ strand #io $ Kleisli $ runConsole consoleEff)
               -- runConsole result just runs in IO. So we ask for a new #io
               -- strand to hold the interpreted console effects...
            -- Rope Rec '[("file",File),("io",Kleisli IO)] (Kleisli IO) String ()
            & weave #file (\runRope -> runRope . loosen . interpFile)
               -- ...that #io strand will be reused by the interpreted File
               -- effects...
            -- Rope Rec '[("io",Kleisli IO)] (Kleisli IO) String ()
            & weave' #io id
               -- ...then set this #io strand to be directly interpreted in the
               -- core...
            -- Rope Rec '[] (Kleisli IO) String ()
            & untwine
            & perform "You"
               -- ...and then we run the core with the input of prog

-- | main details every strand, but we can skip the #io strand and the merging
-- strand an directly interpret every strand in the core effect
altMain :: IO ()
altMain = prog & loosen
               & mergeStrands #console #warnConsole
               & weave' #console (Kleisli . runConsole)
               & weave' #file    (Kleisli . runFile)
               & untwine
               & perform "You"
