{-# LANGUAGE DataKinds #-}
module Control.Biegunka.Options
  ( options
  , parser
  , constructorOptions
  , constructorOption
  , constructorName
  , transformConstructor
  ) where

import Control.Lens
import Control.Monad ((>=>))
import Data.Char (isUpper, toLower)
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Data.Data (Data, toConstr)
import Options.Applicative hiding ((&))
import System.Exit (exitWith)

import Control.Biegunka.Biegunka (biegunka, confirm)
import Control.Biegunka.Settings (Settings, Mode(Offline), mode)
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(Sources))
import Control.Biegunka.Check (check)
import Control.Biegunka.Script (Script)

type Runner a = (Settings () -> Settings ()) -> Script Sources () -> IO a

-- | Run constructed parser
options :: Data a => [a] -> IO (a, Runner b)
options xs = execParser (info (helper <*> parser xs) fullDesc)

-- | Create a parser for a list of data constructors, interpreters, and modes
parser :: Data a => [a] -> Parser (a, Runner b)
parser xs = (\os i f -> (os, \g -> biegunka (g . f) i >=> exitWith))
  <$> constructorOptions xs
  <*> interpreters
  <*> modes
 where
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

-- | Create a parser for a list of data constructors
constructorOptions :: Data a => [a] -> Parser a
constructorOptions = asum . mapAccumL_ constructorOption where

-- | 'mapAccumL' with a 'Monoid'
mapAccumL_ :: (Traversable t, Monoid m) => (m -> a -> (m, b)) -> t a -> t b
mapAccumL_ f = snd . mapAccumL f mempty

-- | Create a parser for a data constructor
constructorOption :: Data a => Map String Int -> a -> (Map String Int, Parser a)
constructorOption acc x = flag' x . long <$> constructorNameSuffixed acc x

-- | Get suffixed constructor name
constructorNameSuffixed :: Data a => Map String Int -> a -> (Map String Int, String)
constructorNameSuffixed acc x =
  let
     n    = constructorName x
     incr = at n.non 0 <<%~ succ
  in
     acc & incr & swap <&> \p -> if p == 0 then n else concat [n, "-", show p]

-- | Get transformed constructor name
constructorName :: Data a => a -> String
constructorName = transformConstructor . show . toConstr

-- | Transform data constructor name
--
-- >>> transformcCnstructor "FooBarBaz"
-- "foo-bar-baz"
transformConstructor :: String -> String
transformConstructor (x:xs) = toLower x : concatMap transformChar xs
 where
  transformChar :: Char -> String
  transformChar y
    | isUpper y = ['-', toLower y]
    | otherwise = [y]
transformConstructor [] = []
