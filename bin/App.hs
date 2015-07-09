{-# LANGUAGE LambdaCase #-}
module App
  ( run
  ) where

import qualified Data.List as List
import           Prelude hiding (init)
import           System.Exit (die)

import           Init (init)
import qualified Json
import           Options (Command(..))
import qualified Run

run :: Either String Command -> IO ()
run = \case
  Right (Init target) -> init target
  Right (Run (Just script) args) -> Run.run script args
  Right (Run Nothing args) ->
    Run.find >>= \case
      [script] -> Run.run script args
      [] -> die "No scripts were found in the tree."
      scripts -> die . List.intercalate "\n" $
        ["Found several scripts:"] ++
        map ("  " ++) scripts ++
        ["Please, pass the one to run as an argument."]
  Right (Json datadir) -> Json.out datadir
  Right (Version version) -> putStrLn version
  Right (Help help) -> putStrLn help
  Left help -> die help
