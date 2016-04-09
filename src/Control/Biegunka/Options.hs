{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Biegunka.Options
  ( Runner
  , runnerOf
  , runner_
  , Environments(..)

  , parser
  ) where

import           Control.Lens
import           Control.Monad ((>=>))
import           Data.Char (isUpper, toLower)
import           Data.Foldable (asum)
import           Data.Proxy (Proxy(Proxy))
import qualified GHC.Generics as G
import           Options.Applicative
import           Prelude hiding (all)
import           System.Exit (exitWith)

import           Control.Biegunka.Biegunka (biegunka)
import           Control.Biegunka.Interpreter (confirm, changes)
import           Control.Biegunka.Settings (Settings, defaultMode, online, offline)
import           Control.Biegunka.Execute (run, runDiff)
import           Control.Biegunka.Language (Scope(Sources), mode)
import           Control.Biegunka.Check (check)
import           Control.Biegunka.Script (Script)


type Runner a = (Settings -> Settings) -> Script 'Sources () -> IO a

-- | Get environment and 'Runner' from the command line options.
runnerOf :: Environments a => IO (a, Runner b)
runnerOf = execParser (parser environments)

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

  runner = (\i f g -> biegunka (g . f) i >=> exitWith)
    <$> interpreters
    <*> modes

  interpreters = asum
    [ flag' diff  (long "diff"     <> help "Show what will change when the script is run")
    , flag' safe  (long "run"      <> help "Run script")
    , flag' check (long "problems" <> help "Show problems")
    , flag' run   (long "force"    <> help "Run script without a confirmation")
    , flag' all   (long "all"      <> help "Show changes, run the script, and show problems")
    , pure all
    ]

  diff = changes <> runDiff
  all  = changes <> safe <> check
  safe = confirm <> run

  modes = asum
    [ flag' offline (long "offline" <> help "Run script offline")
    , flag' online  (long "online"  <> help "Run script online")
    , pure (set mode defaultMode)
    ]

-- | List of possible environments.
--
-- The strings `environments` returns must be unique.
class Environments a where
  environments :: Parser a
  default environments :: (r ~ G.Rep a, G.Generic a, GEnvironment r) => Parser a
  environments = fmap G.to (genv Option { optionName = Nothing })

class GEnvironment p where
  genv :: Option -> Parser (p a)

-- | Datatype declaration's metadata is ignored.
instance GEnvironment f => GEnvironment (G.D1 c f) where
  genv = fmap G.M1 . genv

-- | Record selector's metadata is ignored.
instance GEnvironment f => GEnvironment (G.S1 c f) where
  genv = fmap G.M1 . genv

-- | Sums are translated to sums of parsers.
instance (GEnvironment f, GEnvironment g) => GEnvironment (f G.:+: g) where
  genv x = fmap G.L1 (genv x) <|> fmap G.R1 (genv x)

-- | Constructor names are converted from camelCase to lisp-case
-- and then used as option names.
instance (G.Constructor c, GEnvironment f) => GEnvironment (G.C1 c f) where
  genv _ = fmap G.M1 (genv Option { optionName = Just (camelCase2hyphens con) })
   where
    con = G.conName (G.M1 Proxy :: G.M1 t c Proxy b)

-- | Constructors without arguments are flags.
instance GEnvironment G.U1 where
  genv Option { optionName } = G.U1 <$ maybe empty (flag' () . long) optionName

-- | Constants are options.
instance Read c => GEnvironment (G.K1 i c) where
  genv Option { optionName } = fmap G.K1 (maybe empty (option auto . long) optionName)

newtype Option = Option
  { optionName :: Maybe String
  } deriving (Show, Eq)

-- Convert data constructor name from CamelCase to hyphen-case
-- for use in command line options.
camelCase2hyphens :: String -> String
camelCase2hyphens (x:xs) = toLower x : concatMap go xs
 where
  go y | isUpper y = ['-', toLower y]
       | otherwise = [y]
camelCase2hyphens [] = []
