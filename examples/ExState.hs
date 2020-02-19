{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Category

import Control.Kernmantle.Rope
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (StateT, execStateT)

--------------------------------------------------------------------------------
-- Effect definition

data Add a b where
  Add :: Add Int ()

type a ~~> b = forall m. (MonadIO m) =>
  AnyRopeWith '[ '("add", Add) ]
              '[ArrowState Int, HasKleisli m] a b

add :: Int ~~> ()
add = strand #add Add

getCounter :: () ~~> Int
getCounter = getA @Int

--------------------------------------------------------------------------------
-- Pipeline definition

pipeline :: () ~~> ()
pipeline = proc () -> do
  add -< 100
  n <- getCounter -< ()
  liftKleisliIO id -< putStrLn ("Total: " <> show n)

  add -< 100
  n <- getCounter -< ()
  liftKleisliIO id -< putStrLn ("Total: " <> show n)

--------------------------------------------------------------------------------
-- Effect interpretation

type CoreEff = Kleisli (StateT Int IO)

interpretAdd :: Add a b -> CoreEff a b
interpretAdd Add = proc n -> do
  k <- getA @Int -< ()
  putA @Int -< n + k

main :: IO ()
main = do
  let Kleisli runPipeline =
        pipeline
          & loosen
          & entwine_ #add interpretAdd
          & untwine
  n <- execStateT (runPipeline ()) 0
  print n
