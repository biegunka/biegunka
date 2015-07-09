{-# LANGUAGE LambdaCase #-}
module App
  ( run
  ) where

import qualified Data.List as List
import           Prelude hiding (init)
import           System.Exit (die, exitWith)
import qualified System.IO as IO

import           Init (init)
import qualified Json
import           Options (Command(..))
import qualified Run

run :: Command -> IO ()
run = \case
  Init target -> init target
  Run (Just script) args -> Run.run script args
  Run Nothing args ->
    Run.find >>= \case
      [script] -> Run.run script args
      [] -> die "No scripts were found in the tree."
      scripts -> die . List.intercalate "\n" $
        ["Found several scripts:"] ++
        map ("  " ++) scripts ++
        ["Please, pass the one to run as an argument."]
  Json datadir -> Json.out datadir
  Version version -> putStrLn version
  Help help h exitcode -> do
    IO.hPutStrLn h help
    exitWith exitcode
