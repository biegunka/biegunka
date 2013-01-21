{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Execute
  ( execute, executeWith
  , dropPriviledges, react, templates, defaultExecution
  , Execution, OnFail(..)
  , BiegunkaException(..)
  ) where

import Control.Applicative ((<$>))
import Control.Exception.Lifted (Exception, SomeException(..), handle, throwIO, try)
import Control.Monad (forM_, unless)
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.Function (fix, on)
import Data.Typeable (Typeable)
import Prelude hiding (dropWhile)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)

import           Control.Lens (Lens, makeLenses, (.=), assign, use)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (StateT, runStateT)
import           Control.Monad.Trans (liftIO)
import           System.Directory (getCurrentDirectory, getHomeDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.Env (getEnv)
import           System.Posix.User (getUserEntryForName, userID, setEffectiveUserID)
import           System.Process (system)
import           Text.StringTemplate (ToSElem(..))

import           Biegunka.DB
import           Biegunka.Language (Script, Layer(..), Command(..), Action(..), Wrapper(..), next)
import           Biegunka.Flatten
import qualified Biegunka.Map as Map
import           Biegunka.State


data OnFail = Ignorant | Ask | Abortive


data Execution t = Execution
  { _dropPriviledges :: Bool
  , _onFail :: OnFail
  , _onFailCurrent :: OnFail
  , _templates :: t
  , _user :: String
  }


makeLenses ''Execution


defaultExecution :: Execution Bool
defaultExecution = Execution
  { _dropPriviledges = False
  , _onFail = Ask
  , _onFailCurrent = Ask
  , _templates = False
  , _user = []
  }


react :: Lens (Execution t) (Execution t) OnFail OnFail
react f e@(Execution {_onFail = x}) = (\y -> e {_onFail = y, _onFailCurrent = y}) <$> f x


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- Supports some options
--
-- @
-- main :: IO ()
-- main = executeWith (defaultExecution & react .~ Ignorant) $ do
--   profile ...
--   profile ...
-- @
executeWith :: ToSElem t => Execution t -> Script Profile a -> IO ()
executeWith execution s = do
  home <- getHomeDirectory
  let s' = infect home (flatten s)
      β = Map.construct s'
  α <- load s'
  getEnv "SUDO_USER" >>= \e -> case e of
    Just sudo -> runStateT (fold s') execution { _user = sudo }
    Nothing -> runStateT (fold s') execution
  removeOrphanFiles α β
  removeOrphanRepos α β
  save β
 where
  removeOrphanFiles = removeOrphan removeFile filepaths
  removeOrphanRepos = removeOrphan removeDirectoryRecursive sources

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x -> unless (x `elem` ys) $ (try (f x) :: IO (Either IOError ())) >> return ()


-- | Execute interpreter with default options
execute :: Script Profile () -> IO ()
execute = executeWith defaultExecution


-- | Custom execptions
data BiegunkaException =
    ShellCommandFailure String -- ^ Shell reports errors
  | SourceEmergingFailure String FilePath Text -- ^ Source emerging routine reports errors
  | ExecutionAbortion -- ^ User aborts script
    deriving (Typeable)


instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
   where
    pretty ExecutionAbortion = "Biegunka has aborted"
    pretty (ShellCommandFailure t) = "Biegunka has failed to execute `" <> T.pack t <> "`"
    pretty (SourceEmergingFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs
instance Exception BiegunkaException


-- | Single command execution and exception handling
fold :: ToSElem t => Free (Command l ()) a -> StateT (Execution t) IO ()
fold (Free command) = do
  try (execute' command) >>= \t -> case t of
    Left (SomeException e) -> do
      liftIO . T.putStrLn $ "FAIL: " <> T.pack (show e)
      use onFailCurrent >>= \o -> case o of
        Ignorant -> ignore command
        Ask -> fix $ \ask -> map toUpper <$> prompt "[I]gnore, [R]etry, [A]bort? " >>= \p -> case p of
          "I" -> ignore command
          "R" -> fold (Free command)
          "A" -> liftIO $ throwIO ExecutionAbortion
          _ -> ask
        Abortive -> liftIO $ throwIO ExecutionAbortion
    _ -> fold (next command)
 where
  prompt msg = liftIO $ putStr msg >> hFlush stdout >> getLine

  ignore S {} = fold (dropWhile skip (next command))
  ignore _    = fold (next command)

  skip P {} = False
  skip S {} = False
  skip (W _ (Free x)) = skip x
  skip _ = True
fold (Pure _) = return ()


-- | Command execution
execute' :: ToSElem t => Command l s a -> StateT (Execution t) IO ()
execute' c = f c
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
  h (Template src dst substitute) = do
    ts <- use templates
    liftIO $ overWriteWith (\s d -> toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  h (Shell p sc) = liftIO $ do
    d <- getCurrentDirectory
    setCurrentDirectory p
    handle (\(SomeException _) -> throwIO $ ShellCommandFailure sc) $ do
      e <- system sc
      case e of
        ExitFailure _ -> throwIO $ ShellCommandFailure sc
        _ -> return ()
    setCurrentDirectory d

  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    try (removeLink dst) :: IO (Either SomeException ()) -- needed because removeLink throws an unintended exception if file is absent
    g src dst


dropWhile :: (Command l s (Free (Command l s) b) -> Bool) -> Free (Command l s) b -> Free (Command l s) b
dropWhile f p@(Free c)
  | f c = dropWhile f (next c)
  | otherwise    = p
dropWhile _ x@(Pure _) = x
