{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Files (execute) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)

import Control.Monad.Free (Free(..))
import System.Directory (copyFile, getHomeDirectory, createDirectoryIfMissing, removeFile)
import System.FilePath ((</>), dropFileName, splitFileName)
import System.Posix.Files (createSymbolicLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.DSL.Files (Files(..), Compiler(..))


execute ∷ Free Files a → FilePath → IO ()
execute script path = run script
 where
  run (Free (Message m x)) = putStrLn m >> run x
  run (Free (RegisterAt dst x)) = overWriteWith createSymbolicLink path (</> dst) >> run x
  run (Free (Link src dst x)) = overWriteWith createSymbolicLink (path </> src) (</> dst) >> run x
  run (Free (Copy src dst x)) = overWriteWith copyFile (path </> src) (</> dst) >> run x
  run (Free (Compile cmp src dst x)) = compileWith cmp (path </> src) dst >> run x
  run (Pure _) = return ()


overWriteWith ∷ (FilePath → FilePath → IO ()) → FilePath → (FilePath → FilePath) → IO ()
overWriteWith f s df =
  do d ← df <$> getHomeDirectory
     createDirectoryIfMissing True $ dropFileName d
     try $ removeFile d ∷ IO (Either SomeException ())
     f s d


compileWith ∷ Compiler → FilePath → FilePath → IO ()
compileWith GHC s df =
  do d ← (</> df) <$> getHomeDirectory
     waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", d] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName s
