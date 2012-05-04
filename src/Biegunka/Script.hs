{-# LANGUAGE BangPatterns #-}
module Biegunka.Script where

import Control.Applicative ((<$>))
import Control.Monad (void, when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (tell)
import Data.Monoid ((<>))
import Data.Set (singleton)
import System.Directory (getHomeDirectory)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.FilePath ((</>))
import qualified Data.ByteString as B

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_
  copy_repo_file = copy_repo_file_

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ fp = doWithFiles (overWriteWith createSymbolicLink) id (</> fp)

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ sfp dfp = doWithFiles (overWriteWith createSymbolicLink) (</> sfp) (</> dfp)

copy_repo_file_ ∷ FilePath → FilePath → Script ()
copy_repo_file_ sfp dfp = doWithFiles (overWriteWith copyFile) (</> sfp) (</> dfp)

overWriteWith f s d = liftIO $ do
  exists ← fileExist d
  when exists $ do
    putStrLn $ "Warning: file " <> d <> " does exist!"
    removeLink d
  f s d

doWithFiles f sf df = Script $ do
  s ← sf <$> ask
  d ← df <$> getHomeDirectory'
  void $ f s d
  tell (singleton d)
  where getHomeDirectory' = liftIO getHomeDirectory

copyFile ∷ FilePath → FilePath → IO ()
copyFile s d = do
  !contents ← B.readFile s
  B.writeFile d contents

message_ ∷ String → Script ()
message_ = Script . putStrLn'
  where putStrLn' = liftIO . putStrLn
