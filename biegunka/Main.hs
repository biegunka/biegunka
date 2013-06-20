module Main where

import           Control.Concurrent (forkFinally, forkIO)
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
  | Run [String]


opts :: ParserInfo BiegunkaCommand
opts = info (helper <*> subcommands) fullDesc
 where
  subcommands = subparser $
    command "init" (info (pure Init) (progDesc "Initialize biegunka script")) <>
    command "run"  (info runCommandParser
      (progDesc "Run biegunka script"))
   where
    runCommandParser = Run <$> arguments Just mempty


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  biegunkaCommand <- customExecParser (prefs showHelpOnError) opts
  case biegunkaCommand of
    Init     -> initialize
    Run args -> run args


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


run :: [String] -> IO ()
run args = do
  let (biegunkaArgs, ghcArgs) = partition (\arg -> "--" == take 2 arg) args
  (stdin', stdout', stderr', pid) <- runInteractiveProcess "runhaskell"
    (ghcArgs ++ [destination, "--all"] ++ biegunkaArgs)
    Nothing
    Nothing
  hSetBuffering stdin' NoBuffering
  anchor <- newEmptyMVar
  listen anchor stdout'
  listen anchor stderr'
  tell stdin'
  takeMVar anchor
  exitcode <- getProcessExitCode pid
  case exitcode of
    Just exitcode' -> exitWith exitcode'
    Nothing        -> exitWith (ExitFailure 1)
 where
  listen mvar handle = forkFinally (forever $ T.hGetContents handle >>= T.putStr)
    (\_ -> putMVar mvar ())
  tell handle = forkIO . forever $ T.getLine >>= T.hPutStrLn handle


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
