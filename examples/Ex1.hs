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
interpConsole :: (IOStrand `InRope` rope)
              => a `Console` b -> a `rope` b
interpConsole cmd = ioStrand $ case cmd of
  GetLine -> const getLine
  PutLine -> putStrLn . (">> "++)

-- | The File effect
data a `File` b where
  GetFile :: FilePath `File` String
  PutFile :: (FilePath, String) `File` ()

-- | Run a File effect in IO
interpFile :: (IOStrand `InRope` rope)
            => a `File` b -> a `rope` b
interpFile cmd = ioStrand $ case cmd of
  GetFile -> readFile
  PutFile -> uncurry writeFile

type ProgArrow a b = forall core rest. (ArrowChoice core)
  => LooseRope ('("console",Console) ': '("file",File) ': rest) core a b

-- | The Arrow program we will want to run
prog :: ProgArrow String ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What file would you like to open?"
  fname <- strand #console GetLine -< ()
  contents <- if fname == ""
    then strand #console GetLine -< ()
    else strand #file GetFile -< fname
  strand #console PutLine -< contents

main = prog & entwine interpConsole
            & entwine interpFile
            & flip untwineIO "You"
