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
import           Data.Foldable (asum)
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

data Script = Run Run | Check

data Run = Dry | Safe | Force | Full


opts :: ParserInfo BiegunkaCommand
opts = info (helper <*> subcommands) fullDesc
 where
  subcommands = subparser $
    command "init" (info (pure Init) (progDesc "Initialize biegunka script")) <>
    command "run"  (info (Script <$> (Run <$> runVariant) <*> otherArguments)
      (progDesc "Run biegunka script (with confirmation by default)")) <>
    command "check"  (info (Script Check <$> otherArguments)
      (progDesc "Check biegunka script"))
   where
    runVariant = asum
      [ flag' Dry   (long "dry"   <> help "Do not do anything, only display logs and stats")
      , flag' Force (long "force" <> help "Do not ask for confirmation")
      , flag' Full  (long "full"  <> help "Do also a dry run and check results afterwards")
      , pure Safe
      ]

    otherArguments = arguments Just mempty


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
  -- Can't use waitForProcess here because runhaskell ignores -threaded
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
toOption (Run Force) = "--run"
toOption (Run Safe)  = "--safe-run"
toOption (Run Dry)   = "--dry-run"
toOption (Run Full)  = "--full"
toOption Check       = "--check"


prompt :: String -> IO Bool
prompt message = do
  putStr $ message ++ " [y/N] "
  hFlush stdout
  response <- getLine
  case map toLower response of
    "y" -> return True
    "n" -> return False
    _   -> prompt message


#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 706
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
#endif
