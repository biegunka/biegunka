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
import           Text.Printf (printf)

import qualified Paths_biegunka as Paths
import qualified Git_biegunka as Git


data Command
  = Init FilePath
  | Run (Maybe FilePath) [String]
  | Json FilePath
  | Version String
  | Help String
    deriving (Show, Eq)

parseArgs :: [String] -> Either String Command
parseArgs = \case
  ["init", x] -> pure (Init x)
  ("run" : "--" : xs) -> pure (Run Nothing xs)
  ("run" : x : "--" : xs) -> pure (Run (Just x) xs)
  ["json"] -> pure (Json defaultBiegunkaDataDirectory)
  ["json", x] -> pure (Json x)
  ["version"] -> pure (Version version)
  ["help"] -> pure (Help help)
  _ -> Left help
 where
  help = List.intercalate "\n"
    [ "biegunka " ++ version
    , ""
    , "biegunka init DIR"
    , "    put Biegunka.hs template in DIR"
    , "biegunka run [SCRIPT | Biegunka.hs]"
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

_Help :: Prism' Command String
_Help = prism' Help (\case Help x -> Just x; _ -> Nothing)
