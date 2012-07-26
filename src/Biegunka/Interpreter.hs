{-# LANGUAGE BangPatterns #-}
module Biegunka.Interpreter
  ( dryRun, execute
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Monoid ((<>))
import Data.Set (Set, singleton)

import qualified Data.ByteString as B
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.DB (Biegunka, create)
import Biegunka.Script


dryRun ∷ Free Script a → FilePath → String
dryRun (Free (Message m x)) path =
  "Message: " <> show m <> "\n" <> dryRun x path
dryRun (Free (LinkRepo dst x)) path =
  "Link repository " <> path <> " to ~/" <> dst <> "\n" <> dryRun x path
dryRun (Free (LinkRepoFile src dst x)) path =
  "Link repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRun x path
dryRun (Free (CopyRepoFile src dst x)) path =
  "Copy repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRun x path
dryRun (Free (Compile cmp src dst x)) path =
  "Compile with " <> show cmp <> " repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRun x path
dryRun (Pure _) _ = ""


execute ∷ Free Script a → FilePath → IO Biegunka
execute script path = create path <$> runReaderT (execWriterT $ runScript script) path
 where
  runScript (Free (Message m x)) =
    liftIO (putStrLn m) >> runScript x
  runScript (Free (LinkRepo dst x)) =
    doWithFiles (overWriteWith createSymbolicLink) id (</> dst) >> runScript x
  runScript (Free (LinkRepoFile src dst x)) =
    doWithFiles (overWriteWith createSymbolicLink) (</> src) (</> dst) >> runScript x
  runScript (Free (CopyRepoFile src dst x)) =
    doWithFiles (overWriteWith copyFile) (</> src) (</> dst) >> runScript x
  runScript (Free (Compile cmp src dst x)) =
    compileWith cmp src dst >> runScript x
  runScript (Pure x) = return x


overWriteWith ∷ (FilePath → FilePath → IO a) → FilePath → FilePath → WriterT (Set FilePath) (ReaderT FilePath IO) a
overWriteWith f s d = liftIO $ do
  exists ← fileExist d
  when exists $ removeLink d
  f s d


doWithFiles ∷ (FilePath → FilePath → WriterT (Set FilePath) (ReaderT FilePath IO) a)
            → (FilePath → FilePath)
            → (FilePath → FilePath)
            → WriterT (Set FilePath) (ReaderT FilePath IO) ()
doWithFiles f sf df = do
  s ← sf <$> ask
  d ← df <$> liftIO getHomeDirectory
  liftIO $ createDirectoryIfMissing True (dropFileName d)
  _ ← f s d
  tell (singleton d)


copyFile ∷ FilePath → FilePath → IO ()
copyFile s d = do
  !contents ← B.readFile s
  B.writeFile d contents


compileWith ∷ Compiler → FilePath → FilePath → WriterT (Set FilePath) (ReaderT FilePath IO) ()
compileWith GHC sf df = do
      s ← (</> dir) <$> ask
      d ← (</> df) <$> liftIO getHomeDirectory
      _ ← liftIO $ waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", d] (Just s) Nothing Nothing Nothing Nothing
      return ()
  where (dir, file) = splitFileName sf
