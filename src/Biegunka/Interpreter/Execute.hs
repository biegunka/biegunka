{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Control.Monad.Writer (WriterT, execWriterT, liftIO, tell)
import Data.Set (Set, singleton)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist, getHomeDirectory, createDirectoryIfMissing, removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), hFlush, stdout, withFile)
import System.Posix.Files (createSymbolicLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.DB (Biegunka, create)
import Biegunka.Repository (Repository(..))
import Biegunka.Script (Script(..), Compiler(..))


execute ∷ Free (Repository a) b → IO Biegunka
execute (Free (Git url path script next)) =
  do update url path
     biegunka ← executeRepo script path
     mappend biegunka <$> execute next
execute (Pure _) = return mempty


executeRepo ∷ Free Script a → FilePath → IO Biegunka
executeRepo script path = create path <$> execWriterT (runScript script)
 where
  runScript (Free (Message m x)) =
    liftIO (putStrLn m) >> runScript x
  runScript (Free (LinkRepo dst x)) =
    overWriteWith createSymbolicLink path (</> dst) >> runScript x
  runScript (Free (LinkRepoFile src dst x)) =
    overWriteWith createSymbolicLink (path </> src) (</> dst) >> runScript x
  runScript (Free (CopyRepoFile src dst x)) =
    overWriteWith copyFile (path </> src) (</> dst) >> runScript x
  runScript (Free (Compile cmp src dst x)) =
    compileWith cmp (path </> src) dst >> runScript x
  runScript (Pure x) = return x


update ∷ String → FilePath → IO ()
update u p =
  do exists ← (||) <$> doesDirectoryExist p <*> doesFileExist p
     unless exists $
       withProgressString ("Clone git repository from " ++ u ++ " to " ++ p ++ "… ") $
         withTempFile $ \h →
           waitForProcess =<< runProcess "git" ["clone", u, p] Nothing Nothing Nothing (Just h) (Just h)
     withProgressString ("Pulling in " ++ p ++ " from origin master… ") $
       withTempFile $ \h →
         waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just p) Nothing Nothing (Just h) (Just h)
 where
  withTempFile = withFile "/tmp/biegunka.errors" WriteMode


withProgressString ∷ String → IO ExitCode → IO ()
withProgressString prompt action = do
  putStr prompt >> hFlush stdout
  result ← action
  case result of
    ExitSuccess → putStrLn "OK!"
    ExitFailure _ → putStrLn "Fail!" >> readFile "/tmp/biegunka.errors" >>= error


overWriteWith ∷ (FilePath → FilePath → IO a)
            → FilePath
            → (FilePath → FilePath)
            → WriterT (Set FilePath) IO ()
overWriteWith f s df =
  do d ← liftIO $
       do d ← df <$> getHomeDirectory
          createDirectoryIfMissing True $ dropFileName d
          try $ removeFile d ∷ IO (Either SomeException ())
          f s d
          return d
     tell $ singleton d


compileWith ∷ Compiler → FilePath → FilePath → WriterT (Set FilePath) IO ()
compileWith GHC s df =
  do d ← (</> df) <$> liftIO getHomeDirectory
     liftIO $ waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", d] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName s
