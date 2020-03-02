{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Here we use the @wander@ function from Traversing (which Rope instanciates)
-- that turn any Control.Lens.Traversal into a profunctor optic traversal. This
-- way we can traverse a datastructure with a Rope and a regular Traversal.

import Control.Arrow
import Control.Kernmantle.Error
import Control.Kernmantle.Rope
import Control.Lens hiding (Traversing)
import Data.Profunctor
import Data.Profunctor.Traversing


data DataSet = DataSet {
  _names :: [String],
  _values :: [Float]
  }

makeLenses ''DataSet

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

-- | Just a shortcut to say our program can run in any rope that supports our
-- effects and whose core implements ArrowChoice and allows us to catch
-- IOExceptions
type a ~~> b =
  AnyRopeWith '[ '("console",Console), '("file",File) ]
              '[ArrowChoice, TryEffect IOException, Traversing]
              a b

getContentsToOutput :: FilePath ~~> String
getContentsToOutput = proc filename -> do
  if filename == ""  -- This branching requires ArrowChoice. That's why we put
                     -- it in our type alias as a condition
    then strand #console GetLine -< ()
    else do
      res <- tryE (strand #file GetFile) -< filename
      returnA -< case res of
        Left e -> "Had an error:\n" ++ displayException (e::IOError)
        Right str -> "I read from " ++ filename ++ ":\n" ++ str

-- | Should permit to implement wander_?
newtype ForgetT r p a b = ForgetT {runForgetT :: p a r}

instance Profunctor p => Profunctor (ForgetT r p) where
  dimap f _ (ForgetT p) = ForgetT $ lmap f p
  {-# INLINE dimap #-}
  lmap f (ForgetT p) = ForgetT $ lmap f p
  {-# INLINE lmap #-}
  rmap _ (ForgetT p) = ForgetT p
  {-# INLINE rmap #-}

instance (Monoid r, Choice p) => Choice (ForgetT r p) where
  left' (ForgetT p) = ForgetT $ rmap (either id (const mempty) ) $ left' p
  {-# INLINE left' #-}
  right' (ForgetT p) = ForgetT $ rmap (either (const mempty) id ) $ right' p
  {-# INLINE right' #-}

instance (Strong p) => Strong (ForgetT r p) where
  first' (ForgetT p) = ForgetT $ rmap fst $ first' p
  {-# INLINE first' #-}
  second' (ForgetT p) = ForgetT $ rmap snd $ second' p
  {-# INLINE second' #-}

-- instance (Monoid r, Traversing p) => Traversing (ForgetT r p) where
--   --traverse' (ForgetT p) = ForgetT $
--   wander f (ForgetT p) = ForgetT $ wander f p ???
--   {-# INLINE wander #-}

-- | Not efficient at all for now
wander_ :: (Traversing p, Monoid b) => (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a () -> p s ()
wander_ f eff = rmap (const ()) $ wander f (rmap (const mempty) eff)

-- | The Arrow program we will want to run
prog :: String ~~> ()
prog = proc name -> do
  strand #console PutLine -< "Hello, " ++ name ++ ". What dataset would you like to open?"
  contents <- getContentsToOutput <<< strand #console GetLine -< ()
  let ds = DataSet ["plop","lala","couic"] [23.1, 4, -434]
  wander_ (names . each) (strand #console PutLine) -< ds
  strand #console PutLine -< contents

-- | main details every strand, but we can skip the #io strand and the merging
-- strand an directly interpret every strand in the core effect
main :: IO ()
main = prog & loosen
            & weave' #console (Kleisli . runConsole)
            & weave' #file    (Kleisli . runFile)
            & runSieveCore "You"
