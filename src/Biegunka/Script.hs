module Biegunka.Script where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Monoid ((<>))
import System.Directory (getHomeDirectory, removeFile)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.FilePath ((</>))

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ fp = Script $ do
  s ← ask
  hd ← liftIO getHomeDirectory
  let d = hd </> fp
  liftIO $ do
    exists ← fileExist d
    when (exists) $ do
      putStrLn $ "Warning: file " <> d <> " does exist!"
      removeLink d
    createSymbolicLink s d
  tell [d]

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ sfp dfp = Script $ do
  rd ← ask
  hd ← liftIO getHomeDirectory
  let s = rd </> sfp
  let d = hd </> dfp
  liftIO $ do
    exists ← fileExist d
    when (exists) $ do
      putStrLn $ "Warning: file " <> d <> " does exist!"
      removeLink d
    createSymbolicLink s d
  tell [d]

message_ ∷ String → Script ()
message_ = Script . liftIO . putStrLn
