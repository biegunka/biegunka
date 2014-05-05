{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Run (or check) biegunka script
module Run (run) where

import           Control.Concurrent (forkIO)
import           Control.Lens hiding ((<.>))
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.List (intercalate, isPrefixOf, partition)
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (Version(..), showVersion)
import           System.Exit (ExitCode(..), exitSuccess, exitWith)
import           System.FilePath.Lens (directory)
import           System.Process
import           System.Info (arch, os, compilerName, compilerVersion)
import           System.IO (Handle, hSetBuffering, BufferMode(..))
import           System.Wordexp (wordexp, nosubst, noundef)

import Paths_biegunka (version)

-- | Runs (or checks) biegunka script.
--
-- Does a couple of smart things:
--
--   * If you happen to have cabal[-dev] sandbox it can automatically detect
--   them if they are located in default locations
--
--   * If script path argument is a directory, then default script name is
--   automatically appended, e.g. @biegunka\/@ becomes @biegunka\/Biegunka.hs@
--
--   * Script path directory name is added to paths where ghc searches for
--   modules (@-i@ option)
run :: [String] -> FilePath -> IO ()
run args target = do
  T.putStrLn logo
  let (biegunkaArgs, ghcArgs) = partition ("--" `isPrefixOf`) args
  packageDBArg <- if any ("-package-db" `isPrefixOf`) ghcArgs
                     then return (Right ())
                     else findPackageDBArg
  packageDBArg^!_Left.act (\db -> putStrLn ("* Found cabal package DB at " ++ db ++ ", using it!"))
  (inh, pid) <- runBiegunkaProcess
         (ghcArgs
      ++ ["-i" ++ target^.directory]
      ++ either (\packageDB -> ["-package-db=" ++ packageDB]) (const []) packageDBArg
      ++ [target]
      ++ biegunkaArgs)
  hSetBuffering inh NoBuffering
  tell inh
  exitcode <- waitForProcess pid
  exit exitcode
 where
  tell handle = forkIO . forever $ T.getLine >>= T.hPutStrLn handle

  exit ExitSuccess =
    exitSuccess
  exit (ExitFailure s) = do
    T.putStrLn $ "Biegunka script exited with exit code " <> T.pack (show s)
    exitWith (ExitFailure s)

runBiegunkaProcess :: [String] -> IO (Handle, ProcessHandle)
runBiegunkaProcess args = do
  (Just inh, Nothing, Nothing, ph) <- createProcess process
  return (inh, ph)
 where
  process = CreateProcess
    { cmdspec      = RawCommand "runhaskell" args
    , cwd          = Nothing
    , env          = Nothing
    , std_in       = CreatePipe
    , std_out      = Inherit
    , std_err      = Inherit
    , close_fds    = True
    , create_group = True
#if __GLASGOW_HASKELL__ >= 708
    , delegate_ctlc = False
#endif
    }

logo :: Text
logo = T.unlines
  [ "   ___  _                    __          "
  , "  / _ )(_)__ ___ ___ _____  / /_____ _   "
  , " / _  / / -_) _ `/ // / _ \\/  '_/ _ `/   "
  , "/____/_/\\__/\\_, /\\_,_/_//_/_/\\_\\\\_,_/  " <> T.pack (showVersion version)
  , "           /___/                         "
  ]

findPackageDBArg :: IO (Either String ())
findPackageDBArg = runEitherT $ do
  findCabalSandbox
  findCabalDevSandbox
 where
  findCabalSandbox    =
    findSandbox $ "cabal-dev/packages-" ++ compilerVersionString compilerVersion ++ "*.conf"
  findCabalDevSandbox =
    findSandbox $
         ".cabal-sandbox/" ++ arch ++ "-" ++ os ++ "-" ++ compilerName ++ "-"
      ++ compilerVersionString compilerVersion ++ "*-packages.conf.d"

  findSandbox :: String -> EitherT String IO ()
  findSandbox pattern = do
    findings <- liftIO $ wordexp (nosubst <> noundef) pattern
    case findings of
      Right [sandbox]
        | sandbox /= pattern -> left sandbox
      Right (sandbox:_:_) -> do
        liftIO . putStrLn $ "* Found multiple sandboxes, going with " ++ sandbox ++ ", sorry!"
        left sandbox
      _ -> right ()

  compilerVersionString = intercalate "." . map show . versionBranch
