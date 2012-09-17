{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka.Interpreter.IO provides fancy command execution infrastructure
module Biegunka.Interpreter.IO
  ( sourceFailure
  , issue
  ) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, SomeException(..), throw, try)
import Data.Char (toUpper)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import Text.Printf (printf)

import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process (runProcess, waitForProcess)

import Biegunka.DSL (Command(..), Compiler(..))


-- | Possible user's reactions on failures
data Response =
    Ignore -- ^ Ignore failure, do nothing
  | Retry -- ^ Retry command (meanwhile user might fix the cause)
  | Abort -- ^ Abort sctipt execution entirely, no cleaning is done


-- | Custom execptions
data BiegunkaException =
    CompilationFailure Compiler FilePath Text -- ^ Compiler reports errors
  | SourceEmergingFailure String FilePath Text -- ^ Source emerging routine reports errors
  | ExecutionAbortion -- ^ User aborts script
    deriving (Typeable)


instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
instance Exception BiegunkaException


pretty ∷ BiegunkaException → Text
pretty ExecutionAbortion = T.pack $ show ExecutionAbortion
pretty (CompilationFailure cmp fp fs) = T.unlines $ map T.pack
  [ printf "%s has failed to compile %s" (show cmp) fp
  , printf "Failures log:\n%s" (T.unpack fs)
  ]
pretty (SourceEmergingFailure up fp fs) = T.unlines $ map T.pack
  [ printf "Biegunka has failed to emerge source %s in %s" up fp
  , printf "Failures log:\n%s" (T.unpack fs)
  ]


sourceFailure ∷ String → FilePath → Text → a
sourceFailure up fp fs = throw $ SourceEmergingFailure up fp fs


-- | Single command execution and exception handling
--
-- Returns True is command is successful, False otherwise
issue ∷ Command l s a → IO Bool
issue command = do
  r ← try $ execute command
  case r of
    Left (SomeException e) → do
      printf "FAIL: %s\n" (show e)
      u ← askUser
      case u of
        Retry → issue command
        Abort → throw ExecutionAbortion
        Ignore → return False
    Right () → return True
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
execute ∷ Command l s a → IO ()
execute command = f command
 where
  f ∷ Command l s a → IO ()
  f (Message m _) = putStrLn m
  f (RegisterAt src dst _) = overWriteWith createSymbolicLink src dst
  f (Link src dst _) = overWriteWith createSymbolicLink src dst
  f (Copy src dst _) = overWriteWith copyFile src dst
  f (Compile cmp src dst _) = compileWith cmp src dst
  f (Template src dst substitute _) =
    overWriteWith (\s d → substitute <$> readFile s >>= T.writeFile d) src dst
  f (S { _update = update })= update
  f (P {}) = return ()

  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    removeLink dst
    g src dst

  compileWith GHC src dst = do
    (ifd,ofd) ← createPipe
    ih ← fdToHandle ifd
    oh ← fdToHandle ofd
    r ← waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing (Just oh)
    case r of
      ExitFailure _ → do
        l ← T.hGetContents ih
        throw $ CompilationFailure GHC src l
      _ → return ()
   where
    (dir, file) = splitFileName src
