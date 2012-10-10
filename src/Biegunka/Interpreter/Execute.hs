{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Interpreter.Execute (execute, sourceFailure) where

import Control.Applicative ((<$>))
import Control.Exception (Exception, SomeException(..), throwIO, try)
import Control.Monad (forM_, unless, when)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Monoid (mempty)
import Data.Typeable (Typeable)
import Prelude hiding (print)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.Posix.Files (setFileMode)

import Control.Monad.Free (Free(..))
import Data.Default (Default)
import System.Directory (getHomeDirectory, removeDirectoryRecursive, removeFile)
import           Control.Lens ((^.))
import           Data.Text.Format (Only(..), format, print)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink, removeLink, setOwnerAndGroup)
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)
import           System.Process (runProcess, waitForProcess)

import           Biegunka.DB
import           Biegunka.DSL
  ( ProfileScript
  , Layer(..), Command(..), Action(..), Compiler(..)
  , script, update
  , foldieM, foldieM_
  )
import qualified Biegunka.Interpreter.Map as Map
import           Biegunka.Interpreter.State


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- @
-- main ∷ IO ()
-- main = execute $ do
--   profile ...
--   profile ...
-- @
execute ∷ (Default s, Default t) ⇒ ProfileScript s t () → IO ()
execute s = do
  home ← getHomeDirectory
  let s' = infect home s
  α ← load
  when (α == mempty) $
    putStrLn "Warning: Biegunka is empty"
  profile s'
  let β = Map.construct s'
  removeOrphanFiles α β
  removeOrphanRepos α β
  save β
 where
  removeOrphanFiles = removeOrphan removeFile filepaths
  removeOrphanRepos = removeOrphan removeDirectoryRecursive sources

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) $ (try (f x) ∷ IO (Either SomeException ())) >> return ()


profile ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → IO ()
profile = foldieM_ $ \(P _ s _) → source s


source ∷ Free (Command Source (Free (Command Files ()) ())) () → IO ()
source = foldieM $ \s → issue s >>= \r → when r $ files (s^.script)


files ∷ Free (Command Files ()) a → IO ()
files = foldieM issue


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
   where
    pretty ExecutionAbortion = "Biegunka has aborted"
    pretty (CompilationFailure cmp fp fs) =
      format "{} has failed to compile {}\nFailures log:\n{}" (show cmp, fp, fs)
    pretty (SourceEmergingFailure up fp fs) =
      format "Biegunka has failed to emerge source {} in {}\nFailures log:\n{}" (up, fp, fs)
instance Exception BiegunkaException


sourceFailure ∷ String → FilePath → Text → IO a
sourceFailure up fp fs = throwIO $ SourceEmergingFailure up fp fs


-- | Single command execution and exception handling
--
-- Returns True is command is successful, False otherwise
issue ∷ Command l s a → IO Bool
issue command = do
  r ← try $ execute' command
  case r of
    Left (SomeException e) → do
      print "FAIL: {}\n" (Only (show e))
      u ← askUser
      case u of
        Retry → issue command
        Abort → throwIO (ExecutionAbortion)
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
execute' ∷ Command l s a → IO ()
execute' command = f command
 where
  f ∷ Command l s a → IO ()
  f (F a _) = h a
  f s@(S {}) = s^.update
  f (P {}) = return ()
  f (W {}) = return ()

  h (Message m) = putStrLn m
  h (RegisterAt src dst) = overWriteWith createSymbolicLink src dst
  h (Link src dst) = overWriteWith createSymbolicLink src dst
  h (Copy src dst) = overWriteWith copyFile src dst
  h (Compile cmp src dst) = compileWith cmp src dst
  h (Template src dst substitute) =
    overWriteWith (\s d → substitute <$> readFile s >>= T.writeFile d) src dst
  h (Mode fp m) = setFileMode fp m
  h (Ownership fp u g) = do
    uid ← userID <$> getUserEntryForName u
    gid ← groupID <$> getGroupEntryForName g
    setOwnerAndGroup fp uid gid

  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    try (removeLink dst) ∷ IO (Either SomeException ()) -- needed because removeLink throws an unintended exception if file is absent
    g src dst

  compileWith GHC src dst = do
    (ifd,ofd) ← createPipe
    ih ← fdToHandle ifd
    oh ← fdToHandle ofd
    r ← waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing (Just oh)
    case r of
      ExitFailure _ → do
        l ← T.hGetContents ih
        throwIO $ CompilationFailure GHC src l
      _ → return ()
   where
    (dir, file) = splitFileName src
