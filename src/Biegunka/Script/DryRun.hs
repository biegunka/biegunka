module Biegunka.Script.DryRun where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (tell)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Biegunka

link_repo_itself ∷ FilePath → Script ()
link_repo_itself fp = Script $ liftIO getHomeDirectory >>= \d → tell ([d </> fp])

link_repo_file ∷ FilePath → FilePath → Script ()
link_repo_file _ dst = Script $ liftIO getHomeDirectory >>= \d → tell ([d </> dst])

message ∷ String → Script ()
message = const (return ())
