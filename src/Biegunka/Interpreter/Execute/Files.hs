{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Files (execute) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Free (Free(..))
import Control.Monad.Writer (WriterT, execWriterT, liftIO, tell)
import System.Directory (copyFile, getHomeDirectory, createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Posix.Files (createSymbolicLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.Script (Script(..), Compiler(..))


execute ∷ Free Script a → FilePath → IO (Map FilePath (Set FilePath))
execute script path = M.singleton path <$> execWriterT (runScript script)
 where
  runScript (Free (Message m x)) =
    liftIO (putStrLn m) >> runScript x
  runScript (Free (RegisterAt dst x)) =
    overWriteWith createSymbolicLink path (</> dst) >> runScript x
  runScript (Free (Link src dst x)) =
    overWriteWith createSymbolicLink (path </> src) (</> dst) >> runScript x
  runScript (Free (Copy src dst x)) =
    overWriteWith copyFile (path </> src) (</> dst) >> runScript x
  runScript (Free (Compile cmp src dst x)) =
    compileWith cmp (path </> src) dst >> runScript x
  runScript (Pure x) = return x


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
     tell $ S.singleton d


compileWith ∷ Compiler → FilePath → FilePath → WriterT (Set FilePath) IO ()
compileWith GHC s df =
  do d ← (</> df) <$> liftIO getHomeDirectory
     liftIO $ waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", d] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName s
