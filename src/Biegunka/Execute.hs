{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Execute
  ( execute, executeWith
  , sourceFailure
  , dropPriviledges, react, templates, defaultExecution
  , Execution, OnFail(..)
  ) where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (Exception, SomeException(..), throwIO, try)
import Control.Monad (forM_, unless, when)
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.Function (fix, on)
import Data.Typeable (Typeable)
import Prelude hiding (print)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)

import           Control.Lens (Lens, makeLenses, (.=), assign, use)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (StateT, runStateT)
import           Control.Monad.Trans (liftIO)
import           System.Directory (getHomeDirectory, removeDirectoryRecursive, removeFile)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink, removeLink, setFileMode, setOwnerAndGroup)
import           System.Posix.Env (getEnv)
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID, setEffectiveUserID)
import           System.Process (runProcess, waitForProcess)
import           Text.StringTemplate (ToSElem(..))

import           Biegunka.DB
import           Biegunka.DSL
  ( Script, Layer(..), Command(..), Action(..), Compiler(..), Wrapper(..)
  , foldieM_
  , OnFail(..)
  )
import           Biegunka.Flatten
import qualified Biegunka.Map as Map
import           Biegunka.State


data Execution t = Execution
  { _dropPriviledges ∷ Bool
  , _onFail ∷ OnFail
  , _onFailCurrent ∷ OnFail
  , _templates ∷ t
  , _pemis ∷ Bool
  , _user ∷ String
  }


makeLenses ''Execution


defaultExecution ∷ Execution Bool
defaultExecution = Execution
  { _dropPriviledges = False
  , _onFail = Ask
  , _onFailCurrent = Ask
  , _templates = False
  , _pemis = True
  , _user = []
  }


react ∷ Lens (Execution t) (Execution t) OnFail OnFail
react f e@(Execution {_onFail = x}) = (\y → e {_onFail = y, _onFailCurrent = y}) <$> f x


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- Supports some options
--
-- @
-- main ∷ IO ()
-- main = executeWith (defaultExecution % react .~ Ignorant) $ do
--   profile ...
--   profile ...
-- @
executeWith ∷ ToSElem t ⇒ Execution t → Script Profile → IO ()
executeWith execution s = do
  home ← getHomeDirectory
  let s' = infect home (flatten s)
      β = Map.construct s'
  α ← load s'
  getEnv "SUDO_USER" >>= \case
    Just sudo → runStateT (foldieM_ issue s') execution { _user = sudo }
    Nothing → runStateT (foldieM_ issue s') execution
  removeOrphanFiles α β
  removeOrphanRepos α β
  save β
 where
  removeOrphanFiles = removeOrphan removeFile filepaths
  removeOrphanRepos = removeOrphan removeDirectoryRecursive sources

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) $ (try (f x) ∷ IO (Either SomeException ())) >> return ()


-- | Execute interpreter with default options
execute ∷ Script Profile → IO ()
execute = executeWith defaultExecution


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
      T.pack (show cmp) <> " has failed to compile " <> T.pack fp <> "\nFailures log:\n" <> fs
    pretty (SourceEmergingFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs
instance Exception BiegunkaException


sourceFailure ∷ String → FilePath → Text → IO a
sourceFailure up fp fs = throwIO $ SourceEmergingFailure up fp fs


-- | Single command execution and exception handling
issue ∷ ToSElem t ⇒ Command l () (Free (Command l ()) ()) → StateT (Execution t) IO ()
issue command = do
  try (execute' command) >>= \case
    Left (SomeException e) → do
      case command of
        S {} → pemis .= False
        _ → return ()
      liftIO . T.putStrLn $ "FAIL: " <> T.pack (show e)
      use onFailCurrent >>= \case
        Ignorant → return ()
        Ask → fix $ \ask → map toUpper <$> prompt "[I]gnore, [R]etry, [A]bort? " >>= \case
          "I" → return ()
          "R" → issue command
          "A" → liftIO $ throwIO (ExecutionAbortion)
          _ → ask
        Abortive → liftIO $ throwIO (ExecutionAbortion)
    _ → return ()
 where
  prompt msg = liftIO $ putStr msg >> hFlush stdout >> getLine


-- | Command execution
execute' ∷ ToSElem t ⇒ Command l s a → StateT (Execution t) IO ()
execute' = \case
  (S' {}) → pemis .= True
  s@(S {}) → f s
  command → use pemis >>= \p → when p $ f command
 where
  f (S _ path _ update _) = liftIO $ update path
  f (F a _) = h a
  f (W (Ignorance True) _) = onFailCurrent .= Ignorant
  f (W (Ignorance False) _) = use onFail >>= assign onFailCurrent
  f (W (User (Just name)) _) = liftIO $ getUserEntryForName name >>= setEffectiveUserID . userID
  f (W (User Nothing) _) = use user >>= liftIO . getUserEntryForName >>= liftIO . setEffectiveUserID . userID
  f _ = return ()

  h (Message m) = liftIO $ putStrLn m
  h (RegisterAt src dst) = liftIO $ overWriteWith createSymbolicLink src dst
  h (Link src dst) = liftIO $ overWriteWith createSymbolicLink src dst
  h (Copy src dst) = liftIO $ overWriteWith copyFile src dst
  h (Compile cmp src dst) = liftIO $ compileWith cmp src dst
  h (Template src dst substitute) = do
    ts ← use templates
    liftIO $ overWriteWith (\s d → toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  h (Mode fp m) = liftIO $ setFileMode fp m
  h (Ownership fp u g) = liftIO $ do
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
      ExitFailure _ → T.hGetContents ih >>= throwIO . CompilationFailure GHC src
      _ → return ()
   where
    (dir, file) = splitFileName src
