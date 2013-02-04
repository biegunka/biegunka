{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Execute
  ( execute, executeWith
  , ExecutionState
  , Templates(..), templates, react, Volubility(..), volubility, Priviledges(..), priviledges
  , BiegunkaException(..)
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Exception.Lifted (Exception, SomeException(..), throwIO, try)
import Data.Char (toUpper)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Typeable (Typeable)
import Prelude hiding (dropWhile)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, tryIOError)

import           Control.Lens hiding (Action)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (StateT, runStateT)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (getCurrentDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import           System.Directory (copyFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.Env (getEnv)
import           System.Posix.User (getEffectiveUserName, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process (system)
import           Text.StringTemplate (ToSElem(..))

import Biegunka.Control (Interpreter(..))
import Biegunka.DB
import Biegunka.Execute.Narrator
import Biegunka.Language (Command(..), Action(..), Wrapper(..), React(..), next)


type Execution a = StateT (Narrative, ExecutionState) IO a


data ExecutionState = ExecutionState
  { _priviledges :: Priviledges
  , _react :: React
  , _reactStack :: [React]
  , _templates :: Templates
  , _userStack :: [String]
  , _volubility :: Volubility
  }

data Priviledges =
    Drop
  | Preserve
    deriving (Show, Read, Eq, Ord)

data Templates = forall t. (ToSElem t) => Templates t

instance Default ExecutionState where
  def = ExecutionState
    { _priviledges = Preserve
    , _react = Asking
    , _reactStack = []
    , _templates = Templates False
    , _userStack = []
    , _volubility = Casual
    }

makeLenses ''ExecutionState



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
executeWith :: ExecutionState -> Interpreter
executeWith execution = I $ \s -> do
  let b = construct s
  a <- load s
  n <- narrator (_volubility execution)
  when (execution ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  runStateT (fold s) (n, execution)
  mapM (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save b


-- | Execute interpreter with default options
execute :: Interpreter
execute = executeWith def


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
    pretty (ShellCommandFailure t) =
      "Biegunka has failed to execute `" <> T.pack t <> "`"
    pretty (SourceEmergingFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs

instance Exception BiegunkaException


-- | Single command execution and exception handling
fold :: Free (Command l ()) a -> Execution ()
fold (Free command) = do
  try (execute' command) >>= \t -> case t of
    Left (SomeException e) -> do
      io . T.putStrLn $ "FAIL: " <> T.pack (show e)
      liftA2 (<|>) (use (_2 . reactStack)) (return <$> use (_2 . react)) >>= \(o:_) -> case o of
        Ignorant -> ignore command
        Asking -> fix $ \ask -> map toUpper <$> prompt "[I]gnore, [R]etry, [A]bort? " >>= \p -> case p of
          "I" -> ignore command
          "R" -> fold (Free command)
          "A" -> io $ throwIO ExecutionAbortion
          _ -> ask
        Abortive -> io $ throwIO ExecutionAbortion
    _ -> fold (next command)
 where
  prompt msg = io $ putStr msg >> hFlush stdout >> getLine

  ignore S {} = fold (dropWhile skip (next command))
  ignore _    = fold (next command)

  skip P {} = False
  skip S {} = False
  skip (W _ (Free x)) = skip x
  skip _ = True
fold (Pure _) = return ()


-- | Command execution
execute' :: Command l s a -> Execution ()
execute' c = case c of
  S url path _ update _ -> do
    narrate (Typical $ "Emerging source: " ++ url)
    io $ update path

  F (Message m) _          -> io $ putStrLn m
  F (RegisterAt src dst) _ -> io $ overWriteWith createSymbolicLink src dst
  F (Link src dst) _       -> io $ overWriteWith createSymbolicLink src dst
  F (Copy src dst) _       -> io $ overWriteWith copyFile src dst
  F (Template src dst substitute) _ -> do
    Templates ts <- use (_2 . templates)
    io $ overWriteWith (\s d -> toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  F (Shell p sc) _         -> io $ do
    d <- getCurrentDirectory
    setCurrentDirectory p
    flip catchIOError (\_ -> throwIO $ ShellCommandFailure sc) $ do
      e <- system sc
      case e of
        ExitFailure _ -> throwIO $ ShellCommandFailure sc
        _ -> return ()
    setCurrentDirectory d

  W (Reacting (Just r)) _  -> _2 . reactStack %= (r :)
  W (Reacting Nothing) _   -> _2 . reactStack %= drop 1
  W (User (Just n)) _      -> io getEffectiveUserName >>= \u -> setUser n >> _2 . userStack %= (u :)
  W (User Nothing) _       -> use (_2 . userStack) >>= \(u:us) -> setUser u >> _2 . userStack .= us

  _ -> return ()
 where
  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst


dropWhile :: (Command l s (Free (Command l s) b) -> Bool) -> Free (Command l s) b -> Free (Command l s) b
dropWhile f p@(Free c)
  | f c = dropWhile f (next c)
  | otherwise    = p
dropWhile _ x@(Pure _) = x


setUser :: MonadIO m => String -> m ()
setUser n = io $ getUserEntryForName n >>= setEffectiveUserID . userID


io :: MonadIO m => IO a -> m a
io = liftIO
