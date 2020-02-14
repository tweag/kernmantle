{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}


-- | In this example, we show how to make an effect that can expose options it
-- wants, then accumulate these options into a regular Parser (from
-- optparse-applicative) which builds a pipeline.

import Prelude hiding (id, (.))

import Control.Kernmantle.Rope
import Control.Arrow
import Control.Category
import Data.Functor.Compose
import Data.Profunctor.Cayley
import Options.Applicative
import Data.Char (toUpper)


data a `GetOpt` b where
  GetOpt :: String  -- ^ Name
         -> String  -- ^ Docstring
         -> String  -- ^ Default value
         -> GetOpt a String  -- ^ Returns final value

type a ~~> b =
  AnyRopeWith '[ '("options", GetOpt), '("options2", GetOpt) ] '[Arrow] a b
-- We use two strands of effects for options, just for the sake of showing they
-- can be merged and interpreted at the same time

pipeline :: () ~~> String
pipeline = proc () -> do
  name <- strand #options (GetOpt "name" "The user's name" "Yves") -< ()
  lastname <- strand #options (GetOpt "lastname" "The user's last name" "Parès") -< ()
  age <- strand #options2 (GetOpt "age" "The user's age" "30") -< ()
  returnA -< "Your name is " ++ name ++ " " ++ lastname ++ " and you are " ++ age ++ "."

-- | The core effect we need to collect all our options and build the
-- corresponding CLI Parser. What we should really run once our strands of
-- effects have been evaluated.  Note that @CoreEff a b = Parser (a -> b)@
type CoreEff = Cayley Parser (->)

-- | Turns a GetOpt into an actual optparse-applicative Parser
interpretGetOpt :: String -> GetOpt a b -> CoreEff a b
interpretGetOpt docPrefix (GetOpt name docstring defVal) = Cayley $ const <$>
  strOption ( long name <> help (docPrefix<>docstring) <>
              metavar (map toUpper name) <> value defVal )

-- | We remove all the strands of effects and get down to the core effect:
interpretedPipeline :: CoreEff () String
interpretedPipeline =
  pipeline & loosen
           & entwine #options (asCore . interpretGetOpt "From options: ")
           & entwine #options2 (asCore . interpretGetOpt "From options2: ")
           & untwine

main :: IO ()
main = do
  runPipeline <- execParser $ info (helper <*> runCayley interpretedPipeline) $
       header "A simple kernmantle pipeline"
    <> progDesc "Doesn't do much, but cares about you"
  putStrLn $ runPipeline ()
