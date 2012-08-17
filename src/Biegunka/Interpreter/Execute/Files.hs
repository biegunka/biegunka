{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Files (execute) where

import Control.Exception (SomeException, try)

import Control.Monad.Free (Free(..))
import System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import System.FilePath (dropFileName, splitFileName)
import System.Posix.Files (createSymbolicLink)
import System.Process (runProcess, waitForProcess)

import Biegunka.DSL.Files (Files(..), Compiler(..))


execute ∷ Free Files a → IO ()
execute (Free (Message m x)) = putStrLn m >> execute x
execute (Free (RegisterAt src dst x)) = overWriteWith createSymbolicLink src dst >> execute x
execute (Free (Link src dst x)) = overWriteWith createSymbolicLink src dst >> execute x
execute (Free (Copy src dst x)) = overWriteWith copyFile src dst >> execute x
execute (Free (Compile cmp src dst x)) = compileWith cmp src dst >> execute x
execute (Pure _) = return ()


overWriteWith ∷ (FilePath → FilePath → IO ()) → FilePath → FilePath → IO ()
overWriteWith f src dst =
  do createDirectoryIfMissing True $ dropFileName dst
     try $ removeFile dst ∷ IO (Either SomeException ())
     f src dst


compileWith ∷ Compiler → FilePath → FilePath → IO ()
compileWith GHC src dst =
  do waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName src
