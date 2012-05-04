module Biegunka.DryRun.Script where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Set (singleton)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_
  copy_repo_file = copy_repo_file_

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ fp = doWithFiles id (</> fp) "Link %s to %s"

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ sfp dfp = doWithFiles (</> sfp) (</> dfp) "Link %s to %s"

copy_repo_file_ ∷ FilePath → FilePath → Script ()
copy_repo_file_ sfp dfp = doWithFiles (</> sfp) (</> dfp) "Copy %s to %s"

doWithFiles ∷ (FilePath → FilePath) → (FilePath → FilePath) → String → Script ()
doWithFiles sf df p = Script $ do
  s ← sf <$> ask
  d ← df <$> getHomeDirectory'
  tell (singleton d)
  putStrLn' $ printf p s d
  where getHomeDirectory' = liftIO getHomeDirectory
        putStrLn' = liftIO . putStrLn

message_ ∷ String → Script ()
message_ _ = return ()
