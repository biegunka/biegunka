{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Execute (execute, BiegunkaException(..)) where

import Control.Applicative
import Control.Monad (when)
import Control.Exception.Lifted (Exception, SomeException(..), throwIO, try)
import Data.Char (toUpper)
import Data.List ((\\))
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Typeable (Typeable)
import System.Exit (ExitCode(..))
import System.IO (hFlush, stdout)
import System.IO.Error (catchIOError, tryIOError)

import           Control.Lens hiding (Action)
import           Data.Default (def)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.State (StateT, evalStateT)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
  ( getCurrentDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory
  , copyFile, createDirectoryIfMissing
  )
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.Env (getEnv)
import           System.Posix.User (getEffectiveUserName, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process (system)

import Biegunka.Control (Interpreter(..))
import Biegunka.DB
import Biegunka.Execute.Narrator
import Biegunka.Execute.State
import Biegunka.Language (Command(..), Action(..), Wrapper(..), React(..))


type Execution a = ReaderT (Narrative, EE) (StateT ES IO) a
type Task l a = [Command l () a]


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- Supports some options
--
-- @
-- main :: IO ()
-- main = execute (def & react .~ Ignorant) $ do
--   profile ...
--   profile ...
-- @
execute :: EE -> Interpreter
execute e = I $ \s -> do
  let b = construct s
  a <- load s
  when (e ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  runTask e s
  mapM (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save b


runTask :: EE -> Task l a -> IO ()
runTask e s = do
  n <- narrator (_volubility e)
  (`evalStateT` def) $ (`runReaderT` (n, e)) (fold s)


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
fold :: [Command l () b] -> Execution ()
fold (c:cs) = do
  try (execute' c) >>= \t -> case t of
    Left (SomeException e) -> do
      io . T.putStrLn $ "FAIL: " <> T.pack (show e)
      liftA2 (<|>) (use reactStack) (return <$> view (_2 . react)) >>= \(o:_) -> case o of
        Ignorant -> ignore c
        Asking -> fix $ \ask -> map toUpper <$> prompt "[I]gnore, [R]etry, [A]bort? " >>= \p -> case p of
          "I" -> ignore c
          "R" -> fold (c:cs)
          "A" -> io $ throwIO ExecutionAbortion
          _ -> ask
        Abortive -> io $ throwIO ExecutionAbortion
    _ -> fold cs
 where
  prompt msg = io $ putStr msg >> hFlush stdout >> getLine

  ignore S {} = fold (dropCommands skip cs)
  ignore _    = fold cs

  skip (P {} : _) = False
  skip (S {} : _) = False
  skip (W {} : cs') = skip cs'
  skip _ = True
fold [] = return ()


-- | Command execution
execute' :: Command l s a -> Execution ()
execute' c = case c of
  S url path _ update _ -> do
    narrate (Typical $ "Emerging source: " ++ url)
    io $ update path

  F (RegisterAt src dst) _ -> io $ overWriteWith createSymbolicLink src dst
  F (Link src dst) _       -> io $ overWriteWith createSymbolicLink src dst
  F (Copy src dst) _       -> io $ overWriteWith copyFile src dst
  F (Template src dst substitute) _ -> do
    Templates ts <- view (_2 . templates)
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

  W (Reacting (Just r)) _  -> reactStack %= (r :)
  W (Reacting Nothing) _   -> reactStack %= drop 1
  W (User (Just n)) _      -> io getEffectiveUserName >>= \u -> setUser n >> userStack %= (u :)
  W (User Nothing) _       -> use userStack >>= \(u:us) -> setUser u >> userStack .= us

  _ -> return ()
 where
  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst


dropCommands :: ([Command l a b] -> Bool) -> [Command l a b] -> [Command l a b]
dropCommands f p@(_:cs)
  | f p = dropCommands f cs
  | otherwise    = p
dropCommands _ [] = []


setUser :: MonadIO m => String -> m ()
setUser n = io $ getUserEntryForName n >>= setEffectiveUserID . userID


io :: MonadIO m => IO a -> m a
io = liftIO
