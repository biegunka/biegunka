{-# LANGUAGE LambdaCase #-}
-- | @biegunka@ tool options
module Options
  ( parseArgs
  , Command(..)
  , _Init
  , _Run
  , _Json
  , _Version
  , _Help
  , scriptName
  , defaultBiegunkaDataDirectory
  ) where

import           Control.Lens
import qualified Data.List as List
import           Data.Version (showVersion)
import           Prelude hiding (init)
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))
import qualified System.IO as IO
import           Text.Printf (printf)

import qualified Paths_biegunka as Paths
import qualified Git_biegunka as Git


data Command
  = Init FilePath
  | Run (Maybe FilePath) [String]
  | Json FilePath
  | Version String
  | Help String IO.Handle ExitCode
    deriving (Show, Eq)

parseArgs :: [String] -> Command
parseArgs = \case
  ["init"] -> Init "."
  ["init", x] -> Init x
  ["run"] -> Run Nothing []
  ("run" : "--" : xs) -> Run Nothing xs
  ("run" : xs@(('-' : _) : _)) -> Run Nothing xs
  ("run" : x : "--" : xs) -> Run (Just x) xs
  ("run" : x : xs) -> Run (Just x) xs
  ["json"] -> Json defaultBiegunkaDataDirectory
  ["json", x] -> Json x
  ["version"] -> Version version
  ["help"] -> Help help IO.stdout ExitSuccess
  _ -> Help help IO.stderr (ExitFailure 1)
 where
  help = List.intercalate "\n"
    [ "biegunka " ++ version
    , ""
    , "biegunka init [DIR]"
    , "    put Biegunka.hs template in DIR"
    , "biegunka run [SCRIPT | Biegunka.hs] [OPTIONS]"
    , "    run SCRIPT with OPTIONS"
    , "biegunka json [DIRECTORY | ~/.biegunka]"
    , "    show JSON with Biegunka data in DIR"
    , "biegunka version"
    , "    show version and exit"
    , "biegunka help"
    , "    show this text and exit"
    ]
  version = printf "%s-%s" (showVersion Paths.version) Git.hash

-- | Filename which @biegunka init@ creates by default
scriptName :: FilePath
scriptName = "Biegunka.hs"

-- | @biegunka list@ and @biegunka generate@ default data directory
defaultBiegunkaDataDirectory :: String
defaultBiegunkaDataDirectory = "~/.biegunka"

_Init :: Prism' Command FilePath
_Init = prism' Init (\case Init x -> Just x; _ -> Nothing)

_Run :: Prism' Command (Maybe FilePath, [String])
_Run = prism' (uncurry Run) (\case Run x y -> Just (x, y); _ -> Nothing)

_Json :: Prism' Command FilePath
_Json = prism' Json (\case Json x -> Just x; _ -> Nothing)

_Version :: Prism' Command String
_Version = prism' Version (\case Version x -> Just x; _ -> Nothing)

_Help :: Prism' Command (String, IO.Handle, ExitCode)
_Help = prism' (\(x, y, z) -> Help x y z) (\case Help x y z -> Just (x, y, z); _ -> Nothing)
