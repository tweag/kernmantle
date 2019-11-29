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

-- | Just show the type of runFile once we lift it as part of the 'Rope'
interpFile :: (rope `EntwinesU` '[ '("io", IO) ])  -- It requires an #io UStrand
                                                   -- to put the interpreted
                                                   -- effect in
           => a `File` b -> a `rope` b
interpFile = strand #io . unary . runFile

type ProgArrow requiredStrands a b = forall strands core.
  ( ArrowChoice core, TightRope strands core `Entwines` requiredStrands )
  => TightRope strands core a b

getContentsToOutput :: ProgArrow '[ '("console",Console), '("warnConsole",Console), '("file",File) ]
                                 FilePath String
getContentsToOutput = proc filename -> do
  if filename == ""
    then strand #console GetLine <<< strand #warnConsole PutLine -< "Reading from stdin"
    else strand #file GetFile -< filename

-- | The Arrow program we will want to run
prog :: ProgArrow '[ '("console",Console), '("warnConsole",Console), '("file",File) ]
                  String ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  strand #console PutLine -< "I read:\n" ++ contents

main = prog & loosen -- We turn prog into a LooseRope, whose effects can be
                     -- executed one after the other
            & mergeStrands #console #warnConsole
               -- We used a second Console effect for warnings in prog. We
               -- redirect it to #console
            & entwine #console (strand #io . unary . runConsole)
               -- runConsole result just runs in IO. So we ask for a new #io
               -- strand to hold the interpreted console effects...
            & entwine #file interpFile
               -- ...that strand will be reused by the interpreted File
               -- effects...
            & entwine #io asCore
               -- ...then set this #io strand to be used as the core...
            & flip untwineUnary "You"
               -- ...and then we run the core

-- | main details every strand, but we skip the #io strand an directly interpret
-- in the core effect
altMain :: IO ()
altMain = prog & loosen
               & mergeStrands #console #warnConsole
               & entwine #console (asCore . unary . runConsole)
               & entwine #file (asCore . unary . runFile)
               & flip untwineUnary "You"
