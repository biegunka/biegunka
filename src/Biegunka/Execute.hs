{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Execute (execute, BiegunkaException(..)) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.List ((\\))
import           Data.Monoid ((<>))
import           Data.Foldable (traverse_)
import           Data.Typeable (Typeable)
import           System.Exit (ExitCode(..))
import           System.IO.Error (catchIOError, tryIOError)

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens hiding (Action)
import           Control.Monad.State (evalStateT, runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (def)
import           Data.Proxy
import           Data.Reflection
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
import           System.Posix.User (getEffectiveUserID, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process (system)

import Biegunka.Control (Interpreter(..), Task)
import Biegunka.DB
import Biegunka.Execute.Narrator
import Biegunka.Execute.Control
import Biegunka.Language (Command(..), Action(..), Wrapper(..), React(..))


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
execute e = I $ \_ s -> do
  let b = construct (concat s)
  a <- load (concat s)
  when (e ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  n <- narrator (_volubility e)
  mapConcurrently (runTask e { _narrative = Just n }) $ if e ^. parallel then s else [concat s]
  mapM (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save b


-- | Run single task with supplied environment
runTask :: EE -> Task l b -> IO ()
runTask e t = reify e ((`evalStateT` def) . runE . asProxyOf (task t))


-- | Thread `s' parameter to 'task' function
asProxyOf :: Execution s () -> Proxy s -> Execution s ()
asProxyOf a _ = a


-- | Custom execptions
data BiegunkaException =
    ShellCommandFailure String
  | SourceEmergingFailure String FilePath Text
    deriving (Typeable)


instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
   where
    pretty (ShellCommandFailure t) =
      "Biegunka has failed to execute `" <> T.pack t <> "`"
    pretty (SourceEmergingFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs

instance Exception BiegunkaException


-- | Run single task command by command
-- Complexity comes from responding to errors. User may control reaction via
-- 'react' lens to 'EE'. Here he would be asked for prompt if needed or just
-- notified about errors if not
task :: forall l b s. Reifies s EE => Task l b -> Execution s ()
task t@(c:cs) = do
  e <- try (command c)
  case e of
    Left e' -> respond e' t >>= task
    Right _ -> task cs
task [] = return ()

-- | If only I could come up with MonadBaseControl instance for Execution
try :: Exception e => Execution s a -> Execution s (Either e a)
try (E ex) = do
  eeas <- io . E.try . runStateT ex =<< get
  case eeas of
    Left e       ->          return (Left e)
    Right (a, s) -> put s >> return (Right a)

-- | Get response from task failure processing
respond :: forall l b s. Reifies s EE
        => SomeException -> Task l b -> Execution s (Task l b)
respond e t = do
  io . putStrLn $ "FAIL: " ++ show e
  rc <- use retryCount
  if rc < _retries (reflect (Proxy :: Proxy s)) then do
    io . putStrLn $ "Retry: " ++ show (rc + 1)
    retryCount += 1
    return t
  else do
    retryCount .= 0
    r <- reaction
    return $ case r of
      Ignorant -> ignore t
      Abortive -> []

-- | Get current reaction setting from environment
-- 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s EE => Execution s React
reaction = head . (++ [_react $ reflect (Proxy :: Proxy s)]) <$> use reactStack

-- | If failure happens to be in emerging 'Source' then we need to skip
-- all related 'Files' operations too.
ignore :: Task l b -> Task l b
ignore (S {} : cs) = dropCommands skip cs
 where
  skip (P {} : _) = False
  skip (S {} : _) = False
  skip (W {} : cs') = skip cs'
  skip _ = True
ignore (_ : cs)    = cs
ignore [] = error "Should not been here."


-- | Single command execution
command :: forall l b s. Reifies s EE => Command l () b -> Execution s ()
command (W (Reacting (Just r)) _) = reactStack %= (r :)
command (W (Reacting Nothing)  _) = reactStack %= drop 1
command (W (User     (Just u)) _) = usersStack %= (u :)
command (W (User     Nothing)  _) = usersStack %= drop 1
command c = do
  let sudoingTV = _sudoing $ reflect (Proxy :: Proxy s)
      runningTV = _running $ reflect (Proxy :: Proxy s)
  xs <- use usersStack
  o  <- action c
  io $ case xs of
    []  -> do
      atomically $ readTVar sudoingTV >>= \s -> if s then retry else writeTVar runningTV True
      o
      atomically $ writeTVar runningTV False
    u:_ -> do
      atomically $ do
        [s, r] <- mapM readTVar [sudoingTV, runningTV]
        if s || r then retry else writeTVar sudoingTV True
      uid  <- getEffectiveUserID
      uid' <- userID <$> getUserEntryForName u
      setEffectiveUserID uid'
      o
      setEffectiveUserID uid
      atomically $ writeTVar sudoingTV False
 where
  action (S _ src dst _ update _) = do
    narrate (Typical $ "Emerging source: " ++ src)
    return $ update dst
  action (F (Link src dst) _) = return $ overWriteWith createSymbolicLink src dst
  action (F (Copy src dst) _) = return $ overWriteWith copyFile src dst
  action (F (Template src dst substitute) _) = do
    let ts = _templates $ reflect (Proxy :: Proxy s)
    return $ case ts of
      Templates ts' -> overWriteWith (\s d -> toStrict . substitute ts' . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  action (F (Shell p sc) _) = return $ do
    d <- getCurrentDirectory
    setCurrentDirectory p
    flip catchIOError (\_ -> throwIO $ ShellCommandFailure sc) $ do
      e <- system sc
      case e of
        ExitFailure _ -> throwIO $ ShellCommandFailure sc
        _ -> return ()
    setCurrentDirectory d
  action _ = return $ return ()

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
