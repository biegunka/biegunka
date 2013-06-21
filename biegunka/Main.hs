{-# LANGUAGE CPP #-}
module Main where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
import           Control.Exception (SomeException, mask, try)
import           Control.Concurrent (ThreadId)
#else
import           Control.Concurrent (forkFinally)
#endif
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad (forever)
import           Data.Char (toLower)
import           Data.List (partition)
import           Data.Monoid (mempty)
import qualified Data.Text.Lazy.IO as T
import           Options.Applicative
import           System.Directory (copyFile, doesFileExist)
import           System.Exit (ExitCode(..), exitWith)
import           System.IO (hFlush, hSetBuffering, BufferMode(..), stdout)
import           System.Process (getProcessExitCode, runInteractiveProcess)

import Paths_biegunka_core


data BiegunkaCommand
  = Init
  | Script Script [String]

data Script = DryRun | Run | Check


opts :: ParserInfo BiegunkaCommand
opts = info (helper <*> subcommands) fullDesc
 where
  subcommands = subparser $
    command "init" (info (pure Init) (progDesc "Initialize biegunka script")) <>
    command "dry-run"  (info (Script DryRun <$> runCommandParser)
      (progDesc "Run biegunka script dry")) <>
    command "run"  (info (Script Run <$> runCommandParser)
      (progDesc "Run biegunka script")) <>
    command "check"  (info (Script Check <$> runCommandParser)
      (progDesc "Check biegunka script"))
   where
    runCommandParser = arguments Just mempty


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  biegunkaCommand <- customExecParser (prefs showHelpOnError) opts
  case biegunkaCommand of
    Init -> initialize
    Script script args -> withScript script args


destination :: FilePath
destination = "Dotfiles.hs"


initialize :: IO ()
initialize = do
  template <- getDataFileName "data/biegunka-init.template"
  destinationExists <- doesFileExist destination
  case destinationExists of
    True -> do
      response <- prompt $ destination ++ " already exists! Overwrite?"
      case response of
        True  -> move template
        False -> do
          putStrLn $ "Failed to initialize biegunka script: Already Exists"
          exitWith (ExitFailure 1)
    False -> move template
 where
  move :: FilePath -> IO ()
  move source = do
    copyFile source destination
    putStrLn $ "Initialized biegunka script at " ++ destination


withScript :: Script -> [String] -> IO ()
withScript script args = do
  let (biegunkaArgs, ghcArgs) = partition (\arg -> "--" == take 2 arg) args
  (stdin', stdout', stderr', pid) <- runInteractiveProcess "runhaskell"
    (ghcArgs ++ [destination, toOption script] ++ biegunkaArgs)
    Nothing
    Nothing
  hSetBuffering stdin' NoBuffering
  stdoutAnchor <- newEmptyMVar
  stderrAnchor <- newEmptyMVar
  listen stdoutAnchor stdout'
  listen stderrAnchor stderr'
  tell stdin'
  takeMVar stdoutAnchor
  takeMVar stderrAnchor
  exitcode <- getProcessExitCode pid
  exitWith (maybe (ExitFailure 1) id exitcode)
 where
  listen mvar handle = forkFinally (forever $ T.hGetContents handle >>= T.putStr)
    (\_ -> putMVar mvar ())
  tell handle = forkIO . forever $ T.getLine >>= T.hPutStrLn handle


toOption :: Script -> String
toOption Run    = "--safe-run"
toOption DryRun = "--dry-run"
toOption Check  = "--check"


prompt :: String -> IO Bool
prompt message = do
  putStr $ message ++ " [y/N] "
  hFlush stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    ""  -> return False
    _   -> prompt message


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
