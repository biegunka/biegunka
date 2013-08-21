{-# LANGUAGE OverloadedStrings #-}
-- | Run (or check) biegunka script
module Run (run) where

import           Control.Concurrent (forkFinally)
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Lens hiding ((<.>))
import           Control.Monad (forever)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.List (intercalate, isPrefixOf, partition)
import           Data.Monoid (Monoid(..), (<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Version (Version(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath.Lens (directory)
import           System.Process (getProcessExitCode, runInteractiveProcess)
import           System.Info (arch, os, compilerName, compilerVersion)
import           System.IO (hSetBuffering, BufferMode(..))
import           System.Wordexp (wordexp, nosubst, noundef)

import Options

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
run :: Script -> [String] -> FilePath -> IO ()
run script args target = do
  T.putStr logo
  let (biegunkaArgs, ghcArgs) = partition ("--" `isPrefixOf`) args
  packageDBArg <- if any ("-package-db" `isPrefixOf`) ghcArgs
                     then return (Right ())
                     else findPackageDBArg
  packageDBArg^!_Left.act (putStrLn . mappend "* Found cabal package DB at ")
  (inh, outh, errh, pid) <- runInteractiveProcess "runhaskell"
         (ghcArgs
      ++ ["-i" ++ target^.directory]
      ++ either (\packageDB -> ["-package-db=" ++ packageDB]) (const []) packageDBArg
      ++ [target, toScriptOption script]
      ++ biegunkaArgs)
    Nothing
    Nothing
  hSetBuffering inh NoBuffering
  -- Can't use 'waitForProcess' here because 'waitForProcess' wants -threaded
  -- but runhaskell ignores -threaded and we want to support runhaskell
  -- because compiling scripts is not cool
  stdoutAnchor <- newEmptyMVar
  stderrAnchor <- newEmptyMVar
  listen stdoutAnchor outh
  listen stderrAnchor errh
  tell inh
  takeMVar stdoutAnchor
  takeMVar stderrAnchor
  exitcode <- getProcessExitCode pid
  exitWith (maybe (ExitFailure 1) id exitcode)
 where
  listen mvar handle = forkFinally (forever $ T.hGetContents handle >>= T.putStr)
    (\_ -> putMVar mvar ())
  tell handle = forkIO . forever $ T.getLine >>= T.hPutStrLn handle

logo :: Text
logo = T.unlines
  [ "   ___  _                    __          "
  , "  / _ )(_)__ ___ ___ _____  / /_____ _   "
  , " / _  / / -_) _ `/ // / _ \\/  '_/ _ `/   "
  , "/____/_/\\__/\\_, /\\_,_/_//_/_/\\_\\\\_,_/  0.2"
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
        liftIO . putStrLn $ "Found multiple sandboxes, going with " ++ sandbox ++ ", sorry!"
        left sandbox
      _ -> right ()

  compilerVersionString = intercalate "." . map show . versionBranch
