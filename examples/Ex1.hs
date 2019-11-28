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
interpConsole :: (rope `Entwines` '[IOStrand])
              => a `Console` b -> a `rope` b
interpConsole cmd = ioStrand $ case cmd of
  GetLine -> const getLine
  PutLine -> putStrLn . (">> "++)

-- | The File effect
data a `File` b where
  GetFile :: FilePath `File` String
  PutFile :: (FilePath, String) `File` ()

-- | Run a File effect in IO
interpFile :: (rope `Entwines` '[IOStrand])
            => a `File` b -> a `rope` b
interpFile cmd = ioStrand $ case cmd of
  GetFile -> readFile
  PutFile -> uncurry writeFile

type ProgArrow requiredStrands a b = forall strands core.
  ( ArrowChoice core, TightRope strands core `Entwines` requiredStrands )
  => TightRope strands core a b

getContentsToOutput :: ProgArrow '[ '("console",Console), '("warnOutput",Console), '("file",File) ]
                                 FilePath String
getContentsToOutput = proc filename -> do
  if filename == ""
    then strand #console GetLine <<< strand #warnOutput PutLine -< "Reading from command line"
    else strand #file GetFile -< filename

-- | The Arrow program we will want to run
prog :: ProgArrow '[ '("console",Console), '("warnOutput",Console), '("file",File) ]
                  String ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  strand #console PutLine -< "I read:\n" ++ contents

main = prog & loosen
            & entwine #console interpConsole
            & entwine #warnOutput interpConsole -- We use a second Console
                                                -- effect for warnings
            & entwine #file interpFile
            & flip untwineIO "You"
