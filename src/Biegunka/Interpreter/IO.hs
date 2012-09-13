{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Interpreter.IO provides fancy command execution infrastructure
module Biegunka.Interpreter.IO (issue) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, SomeException(..), throw, try)
import Data.Char (toUpper)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import qualified Data.Text.Lazy.IO as T
import           System.Directory (copyFile, createDirectoryIfMissing, removeFile)
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (runProcess, waitForProcess)

import Biegunka.DSL (Command(..), Files, Compiler(..))


-- | Possible user's reactions on failures
data Response =
    Ignore -- ^ Ignore failure, do nothing
  | Retry -- ^ Retry command (meanwhile user might fix the cause)
  | Abort -- ^ Abort sctipt execution entirely, no cleaning is done


-- | Custom execptions
data BiegunkaException =
    CompilationFailure Compiler FilePath -- ^ Compiler reports errors
  | ExecutionAbortion -- ^ User aborts script
    deriving (Show, Typeable)


instance Exception BiegunkaException


-- | Single command execution and exception handling
issue ∷ Command Files () a → IO ()
issue command = do
  r ← try $ execute command
  case r of
    Left (SomeException e) → do
      printf "Exception raised: %s\n" (show e)
      u ← askUser
      case u of
        Retry → issue command
        Abort → throw ExecutionAbortion
        Ignore → return ()
    Right () → return ()
 where
  askUser = do
    putStr "[I]gnore, [R]etry, [A]bort? "
    hFlush stdout
    m ← getLine
    case map toUpper m of
      "I" → return Ignore
      "R" → return Retry
      "A" → return Abort
      _ → askUser


-- | Command execution
execute ∷ Command Files () a → IO ()
execute command = f command
 where
  f ∷ Command Files () a → IO ()
  f (Message m _) = putStrLn m
  f (RegisterAt src dst _) = overWriteWith createSymbolicLink src dst
  f (Link src dst _) = overWriteWith createSymbolicLink src dst
  f (Copy src dst _) = overWriteWith copyFile src dst
  f (Compile cmp src dst _) = compileWith cmp src dst
  f (Template src dst substitute _) = substitute <$> readFile src >>= T.writeFile dst

  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    removeFile dst
    g src dst

  compileWith GHC src dst = do
    r ← waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing Nothing
    case r of
      ExitFailure _ → throw $ CompilationFailure GHC src
      _ → return ()
   where
    (dir, file) = splitFileName src
