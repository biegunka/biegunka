{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Control.Biegunka.Options
  ( Runner
  , runnerOf
  , runner_
  , Environments(..)

  , parser
  , fromEnvironments
  ) where

import Control.Lens hiding (from)
import Control.Monad ((>=>))
import Data.Char (isUpper, toLower)
import Data.Foldable (asum)
import GHC.Generics (Generic, M1(M1), D1, C1, Constructor(conName), (:+:)(L1, R1), U1(U1), Rep, from)
import Options.Applicative
import System.Exit (exitWith)

import Control.Biegunka.Biegunka (biegunka, confirm)
import Control.Biegunka.Settings (Settings, Mode(Offline), mode)
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(Sources))
import Control.Biegunka.Check (check)
import Control.Biegunka.Script (Script)


type Runner a = (Settings () -> Settings ()) -> Script 'Sources () -> IO a

-- | Get environment and 'Runner' from the command line options.
runnerOf :: Environments a => p a -> IO (a, Runner b)
runnerOf xs = execParser (parser (fromEnvironments xs))

-- | Get 'Runner' from the command line options.
runner_ :: IO (Runner b)
runner_ = fmap snd (execParser (parser (pure ())))

-- | Construct the parser for interpreters and modes given a parser
-- for environments.
parser :: Parser a -> ParserInfo (a, Runner b)
parser p = info (helper <*> go) fullDesc
 where
  go = (,) <$> p
           <*> runner

  runner = (\i f -> \g -> biegunka (g . f) i >=> exitWith)
    <$> interpreters
    <*> modes

  interpreters = asum
    [ flag' dryRun  (long "changes"  <> help "List script changes")
    , flag' safeRun (long "run"      <> help "Run script")
    , flag' check   (long "problems" <> help "List problematic filepaths")
    , flag' run     (long "force"    <> help "Run script without confirmation")
    , flag' allRun  (long "all"      <> help "A combination of --dry-run, --run, and --problems")
    , pure allRun
    ]

  allRun  = dryRun <> safeRun <> check
  safeRun = confirm <> run

  modes = asum
    [ flag' (set mode Offline) (long "offline" <> help "Run script offline")
    , flag' id                 (long "online"  <> help "Run script online")
    , pure id
    ]

-- | Contruct a parser from a list of environments.
fromEnvironments :: Environments a => p a -> Parser a
fromEnvironments = asum . map (\(name, val) -> flag' val (long name)) . environments

-- | List of possible environments.
--
-- The strings `environments` returns must be unique.
class Environments a where
  environments :: proxy a -> [(String, a)]
  default environments :: (Bounded a, Enum a, Generic a, GEnvironment (Rep a)) => proxy a -> [(String, a)]
  environments _ = map (\a -> (genvironment (from a), a)) [minBound .. maxBound :: a]

class GEnvironment p where
  genvironment :: p a -> String

instance (GEnvironment f, GEnvironment g) => GEnvironment (f :+: g) where
  genvironment (L1 x) = genvironment x
  genvironment (R1 x) = genvironment x

instance GEnvironment f => GEnvironment (D1 c f) where
  genvironment (M1 x) = genvironment x

-- `GEnvironment f` constraint ensures that it's impossible
-- to construct invalid instances with `Generics`
instance (Constructor c, GEnvironment f) => GEnvironment (C1 c f) where
  genvironment m@(M1 _) = camelCase2hyphens (conName m)

-- because unary constructors are the only ones for which derivation works!
instance GEnvironment U1 where
  genvironment U1 = undefined

-- Convert data constructor name from CamelCase to hyphen-case
-- for use in command line options.
camelCase2hyphens :: String -> String
camelCase2hyphens (x:xs) = toLower x : concatMap go xs
 where
  go y | isUpper y = ['-', toLower y]
       | otherwise = [y]
camelCase2hyphens [] = []
