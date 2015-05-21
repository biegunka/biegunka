{-# LANGUAGE OverloadedStrings #-}
-- | Run (or check) biegunka script
module Run (run) where

import           Control.Exception (catchJust)
import           Control.Concurrent (forkIO)
import           Control.Lens hiding ((<.>))
import           Control.Monad (guard)
import           Data.Function (fix)
import           Data.List (isPrefixOf, partition)
import           Data.Monoid ((<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           Data.Version (showVersion)
import           System.Exit (ExitCode(..), exitSuccess, exitWith)
import           System.FilePath.Lens (directory)
import           System.Process
import qualified System.IO as IO
import qualified System.IO.Error as IO

import           Paths_biegunka (version)
import qualified Git_biegunka as Git

-- | Runs (or checks) biegunka script.
--
-- Does a couple of smart things:
--
--   * Uses @cabal exec@ to run @runhaskell@ inside the cabal sandbox automatically
--
--   * If script path argument is a directory, then default script name is
--   automatically appended, e.g. @biegunka\/@ becomes @biegunka\/Biegunka.hs@
--
--   * Script path directory name is added to paths where ghc searches for
--   modules (@-i@ option)
run :: [String] -> FilePath -> IO ()
run args target = do
  Text.putStrLn logo
  let (biegunkaArgs, ghcArgs) = partition ("--" `isPrefixOf`) args
  (inh, pid) <- runBiegunkaProcess
         (ghcArgs
      ++ ["-i" ++ view directory target]
      ++ [target]
      ++ biegunkaArgs)
  IO.hSetBuffering inh IO.NoBuffering
  tell inh
  exitcode <- waitForProcess pid
  exit exitcode
 where
  tell handle =
    forkIO (fix (\loop ->
      catchJust
        (guard . IO.isEOFError)
        (do line <- Text.getLine
            Text.hPutStrLn handle line
            loop)
        (\_ -> return ())))

  exit ExitSuccess =
    exitSuccess
  exit (ExitFailure s) = do
    Text.putStrLn $ "Biegunka script exited with exit code " <> Text.pack (show s)
    exitWith (ExitFailure s)

runBiegunkaProcess :: [String] -> IO (IO.Handle, ProcessHandle)
runBiegunkaProcess args = do
  (Just inh, Nothing, Nothing, ph) <- createProcess process
  return (inh, ph)
 where
  process = CreateProcess
    { cmdspec       = RawCommand "cabal" (["exec", "runhaskell", "--"] ++ args)
    , cwd           = Nothing
    , env           = Nothing
    , std_in        = CreatePipe
    , std_out       = Inherit
    , std_err       = Inherit
    , close_fds     = True
    , create_group  = False
    , delegate_ctlc = True
    }

logo :: Text
logo = Text.unlines
  [ "   ___  _                    __          "
  , "  / _ )(_)__ ___ ___ _____  / /_____ _   "
  , " / _  / / -_) _ `/ // / _ \\/  '_/ _ `/   "
  , "/____/_/\\__/\\_, /\\_,_/_//_/_/\\_\\\\_,_/  " <> Text.pack (showVersion version ++ "-" ++ Git.hash)
  , "           /___/                         "
  ]
