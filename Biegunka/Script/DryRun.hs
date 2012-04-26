module Biegunka.Script.DryRun where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (tell)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Biegunka

data Host = Root | Home

dir ∷ Host → IO FilePath
dir Root = return ""
dir Home = getHomeDirectory

repoTo ∷ Host → FilePath → Script ()
repoTo h fp = Script $ do
  d <- liftIO (dir h)
  tell ([d </> fp])

fromRepoTo ∷ Host → (FilePath, FilePath) → Script ()
fromRepoTo h (_, fp) = Script $ do
  d <- liftIO (dir h)
  tell ([d </> fp])
