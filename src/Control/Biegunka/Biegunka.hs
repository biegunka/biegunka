{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka
    -- * Auxiliary
  , expandHome
  ) where

import           Control.Lens
import           Data.Bool (bool)
import qualified Data.List as List
import           Data.Version (showVersion)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import           Control.Biegunka.Interpreter (Interpreter(..))
import           Control.Biegunka.Language
import qualified Control.Biegunka.Logger as Logger
import           Control.Biegunka.Script
  (Script, defaultMAnnotations, defaultAnnotations, evalScript)
import           Control.Biegunka.Settings
import qualified System.IO as IO

import qualified Git_biegunka as Git
import qualified Paths_biegunka as Paths


-- | Entry point into the library
biegunka
  :: (Settings -> Settings) -- ^ User defined settings
  -> Interpreter            -- ^ Combined interpreters
  -> Script 'Sources ()     -- ^ Script to interpret
  -> IO ExitCode
biegunka (($ defaultSettings) -> c) (I interpret) script = do
  rr <- views runRoot expandHome c
  br <- views biegunkaRoot expandHome c
  Logger.with $ \l -> do
    Logger.write IO.stdout l (info rr br)
    let annotatedScript = evalScript defaultMAnnotations (set runRoot rr defaultAnnotations) script
        c' = c
          & runRoot      .~ rr
          & biegunkaRoot .~ br
          & _logger      ?~ l
    interpret c' annotatedScript (return ExitSuccess)
 where
  info rr br = List.intercalate "\n" $
    [ "   ___  _                    __          "
    , "  / _ )(_)__ ___ ___ _____  / /_____ _   "
    , " / _  / / -_) _ `/ // / _ \\/  '_/ _ `/   "
    , "/____/_/\\__/\\_, /\\_,_/_//_/_/\\_\\\\_,_/  " ++ version
    , "           /___/                         "
    , ""
    , "* Relative filepaths are deemed relative to " ++ rr
    , "* Data will be saved in "                     ++ br
    ] ++
    bool [] ["* Offline mode"] (has (mode._Offline) c)
   where
    version = showVersion Paths.version ++ "-" ++ Git.hash


-- | Expand \"~\" at the start of the path
expandHome :: String -> IO String
expandHome ('~' : (splitUser -> (user, '/' : xs))) = getHome user <&> (</> xs)
expandHome ('~' : (splitUser -> (user, "")))       = getHome user
expandHome x = return x

-- | Break the path on the first '/'
splitUser :: String -> (String, String)
splitUser = break (== '/')

-- | Get home directory for user by name. If the name is empty return the value
-- of HOME environment variable
getHome :: String -> IO FilePath
getHome "" = Posix.getEnvDefault "HOME" ""
getHome user = fmap Posix.homeDirectory (Posix.getUserEntryForName user)
