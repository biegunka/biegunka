module Biegunka.DryRun.Script where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (tell)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ fp = Script $ liftIO getHomeDirectory >>= \d → tell [d </> fp]

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ _ dst = Script $ liftIO getHomeDirectory >>= \d → tell [d </> dst]

message_ ∷ String → Script ()
message_ = const (return ())
