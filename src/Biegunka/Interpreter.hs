{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter
  ( dryRun, execute
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless, when)
import Data.Monoid (Monoid(..), (<>))
import Data.Set (Set, singleton)

import qualified Data.ByteString as B
import Control.Monad.Free (Free(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.IO (Handle, IOMode(WriteMode), hFlush, stdout, withFile)
import System.Posix.Files (createSymbolicLink, fileExist, removeLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.DB (Biegunka, create)
import Biegunka.Repository (Repository(..))
import Biegunka.Script (Script(..), Compiler(..))


dryRun ∷ Free (Repository a) b → String
dryRun (Free (Git url path script next)) =
  "Setup repository " <> url <> " at " <> path <> "\n" <> dryRunRepo script path <> "\n" <> dryRun next
dryRun (Pure _) = ""


dryRunRepo ∷ Free Script a → FilePath → String
dryRunRepo (Free (Message m x)) path =
  "Message: " <> show m <> "\n" <> dryRunRepo x path
dryRunRepo (Free (LinkRepo dst x)) path =
  "Link repository " <> path <> " to ~/" <> dst <> "\n" <> dryRunRepo x path
dryRunRepo (Free (LinkRepoFile src dst x)) path =
  "Link repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRunRepo x path
dryRunRepo (Free (CopyRepoFile src dst x)) path =
  "Copy repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRunRepo x path
dryRunRepo (Free (Compile cmp src dst x)) path =
  "Compile with " <> show cmp <> " repository file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> dryRunRepo x path
dryRunRepo (Pure _) _ = ""


execute ∷ Free (Repository a) b → IO Biegunka
execute (Free (Git url path script next)) =
  do update url path
     biegunka ← executeRepo script path
     mappend biegunka <$> execute next
execute (Pure _) = return mempty


executeRepo ∷ Free Script a → FilePath → IO Biegunka
executeRepo script path = create path <$> runReaderT (execWriterT $ runScript script) path
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


update ∷ String → FilePath → IO ()
update u p = do
  exists ← (||) <$> doesDirectoryExist p <*> doesFileExist p
  unless exists $
    withProgressString ("Clone git repository from " ++ u ++ " to " ++ p ++ "… ") $
      withTempFile $ \h →
        waitForProcess =<< runProcess "git" ["clone", u, p] Nothing Nothing Nothing (Just h) (Just h)
  withProgressString ("Pulling in " ++ p ++ " from origin master… ") $
    withTempFile $ \h →
      waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just p) Nothing Nothing (Just h) (Just h)


withProgressString ∷ String → IO ExitCode → IO ()
withProgressString hello μ = do
  putStr hello >> hFlush stdout
  result ← μ
  printResult result
  where printResult ∷ ExitCode → IO ()
        printResult ExitSuccess = putStrLn "OK!"
        printResult (ExitFailure _) = do
          putStrLn "Fail!"
          errors ← readFile "/tmp/biegunka.errors"
          error errors


withTempFile ∷ (Handle → IO α) → IO α
withTempFile = withFile "/tmp/biegunka.errors" WriteMode


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
