{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

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
           -> AnyRopeWith '[ '("io", FromUnary IO) ] '[] a b
              -- We give our list of requirements. Interpreting File effects
              -- will require an #io Strand to put the IO actions in. No
              -- requirements on the core.
interpFile = strand #io . unary . runFile

-- | Just a shortcut to say our program can run in any rope that supports our
-- effects and whose core implements ArrowChoice
type a ~~> b =
  AnyRopeWith '[ '("console",Console), '("warnConsole",Console), '("file",File) ]
              '[ArrowChoice]
              a b

getContentsToOutput :: FilePath ~~> String
getContentsToOutput = proc filename -> do
  if filename == ""  -- This branching requires ArrowChoice. That's why we put
                     -- it in our type alias as a condition
    then strand #console GetLine <<< strand #warnConsole PutLine -< "Reading from stdin"
    else strand #file GetFile -< filename

-- | The Arrow program we will want to run
prog :: String ~~> ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  strand #console PutLine -< "I read:\n" ++ contents

main :: IO ()
main = prog & loosen -- We tell that prog should be a TightRope, and at the same
                     -- time turn it into a LooseRope, whose effects can be
                     -- executed one after the other
            & mergeStrands #console #warnConsole
               -- We used a second Console effect for warnings in prog. We
               -- redirect it to #console
            & entwine #console (strand #io . unary . runConsole)
               -- runConsole result just runs in IO. So we ask for a new #io
               -- strand to hold the interpreted console effects...
            & entwine #file interpFile
               -- ...that #io strand will be reused by the interpreted File
               -- effects...
            & entwine #io asCore
               -- ...then set this #io strand to be directly interpreted in the
               -- core...
            & runUnaryCore "You"
               -- ...and then we run the core with the input of prog

-- | main details every strand, but we can skip the #io strand and the merging
-- strand an directly interpret every strand in the core effect
altMain :: IO ()
altMain = prog & loosen
               & mergeStrands #console #warnConsole
               & entwine #console (asCore . unary . runConsole)
               & entwine #file (asCore . unary . runFile)
               & runUnaryCore "You"
