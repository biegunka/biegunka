{-# LANGUAGE BangPatterns #-}
-- | Biegunka.Script module is default script engine. It does actual work.
module Biegunka.Script () where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Writer (WriterT, tell)
import Data.Set (Set, singleton)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.Process (runProcess, waitForProcess)
import qualified Data.ByteString as B

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_
  copy_repo_file = copy_repo_file_
  compile_with = compile_with_

message_ ∷ String → Script ()
message_ = Script . liftIO . putStrLn

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ fp = doWithFiles (overWriteWith createSymbolicLink) id (</> fp)

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ sfp dfp = doWithFiles (overWriteWith createSymbolicLink) (</> sfp) (</> dfp)

copy_repo_file_ ∷ FilePath → FilePath → Script ()
copy_repo_file_ sfp dfp = doWithFiles (overWriteWith copyFile) (</> sfp) (</> dfp)

overWriteWith ∷ MonadIO m ⇒ (FilePath → FilePath → IO a) → FilePath → FilePath → m a
overWriteWith f s d = liftIO $ do
  exists ← fileExist d
  when exists $ removeLink d
  f s d

doWithFiles ∷ (FilePath → FilePath → WriterT (Set FilePath) (ReaderT FilePath IO) a)
            → (FilePath → FilePath)
            → (FilePath → FilePath)
            → Script ()
doWithFiles f sf df = Script $ do
  s ← sf <$> ask
  d ← df <$> getHomeDirectory'
  liftIO $ createDirectoryIfMissing True (dropFileName d)
  _ ← f s d
  tell (singleton d)

copyFile ∷ FilePath → FilePath → IO ()
copyFile s d = do
  !contents ← B.readFile s
  B.writeFile d contents

compile_with_ ∷ Compiler → FilePath → FilePath → Script ()
compile_with_ GHC sf df = Script $ do
      s ← (</> dir) <$> ask
      d ← (</> df) <$> getHomeDirectory'
      _ ← liftIO $ waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", d] (Just s) Nothing Nothing Nothing Nothing
      return ()
  where (dir, file) = splitFileName sf

getHomeDirectory' ∷ WriterT (Set FilePath) (ReaderT FilePath IO) FilePath
getHomeDirectory' = liftIO getHomeDirectory
