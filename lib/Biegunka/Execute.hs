{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- | Real run interpreter
module Biegunka.Execute (run, execute) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.Foldable (traverse_)
import           Data.List ((\\))
import           Prelude hiding (log)
import           System.Exit (ExitCode(..))
import           System.IO.Error (tryIOError)

import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically)
import           Control.Lens hiding (op)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (def)
import           Data.Proxy
import           Data.Reflection
import           Data.Functor.Trans.Tagged
import qualified Data.Set as S
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
  ( removeDirectoryRecursive, removeFile, copyFile, createDirectoryIfMissing )
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.Env (getEnv)
import           System.Posix.User (getEffectiveUserID, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process

import Biegunka.Control (Interpreter(..), interpret, logger)
import Biegunka.DB
import Biegunka.Execute.Control
import Biegunka.Execute.Exception
import Biegunka.Execute.Describe (describe, action, exception, retryCounter)
import Biegunka.Language (EL(..), A(..), S(..), M(..), React(..), peek)
import Biegunka.Execute.Schedule (runTask, schedule)
import Biegunka.Script


-- | Real run interpreter
run :: (EE () -> EE ()) -> Interpreter
run e = interpret $ \c s -> do
  let b = construct s
  a <- load c s
  (controls .~ c -> e') <- initializeSTM (e def)
  dropPriviledges e'
  runTask e' def newTask s
  atomically (writeTQueue (e'^.stm.work) (Stop 0)) -- Can we assume script starts from id 0?
  schedule (e'^.stm.work)
  mapM_ (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM_ (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save c b

-- | Real run interpreter
execute :: (EE () -> EE ()) -> Interpreter
execute = run
{-# DEPRECATED execute "Please, use `run'" #-}

dropPriviledges :: EE a -> IO ()
dropPriviledges e =
  case e^.priviledges of
    Drop     -> getEnv "SUDO_USER" >>= traverse_ setUser
    Preserve -> return ()
 where
  setUser n = getUserEntryForName n >>= setEffectiveUserID . userID


-- | Run single task command by command
--
-- "Forks" on 'Task' 'True'.
-- Note: current thread continues to execute what's inside task, but all the other stuff is queued
--
-- Complexity comes from forking and responding to errors. Otherwise that is dead simple function
task :: Reifies t (EE STM) => Free (EL SA s) a -> Execution t ()
task (Free (EP _ _ b d)) = do
  newTask d
  task b
task (Free c@(ES _ _ b d)) = do
  newTask d
  try (command c) >>= \e -> case e of
    Left e' -> retry e' >>= \r -> case r of
      Retry    -> task (Free (Pure () <$ c))
      Abortive -> return ()
      Ignorant -> task b
    Right _ -> task b
task a@(Free c) =
  try (command c) >>= \e -> case e of
    Left e' -> retry e' >>= \r -> case r of
      Retry    -> task a
      Abortive -> return ()
      Ignorant -> task (peek c)
    Right _ -> task (peek c)
task (Pure _) = return ()


-- | If only I could come up with MonadBaseControl instance for Execution
try :: Exception e => Execution s a -> Execution s (Either e a)
try (TagT ex) = do
  eeas <- liftIO . E.try . runStateT ex =<< get
  case eeas of
    Left e       ->          return (Left e)
    Right (a, s) -> put s >> return (Right a)

-- | Get response from task failure processing
--
-- Possible responses: retry command execution or ignore failure or abort task
retry :: forall s. Reifies s (EE STM) => SomeException -> Execution s React
retry exc = do
  log <- reflected <&> \e -> e^.controls.logger
  liftIO . log . describe $ exception exc
  rc <- retryCount <<%= (+1)
  if rc < _retries (reflect (Proxy :: Proxy s)) then do
    liftIO . log . describe $ retryCounter (rc + 1)
    return Retry
  else do
    retryCount .= 0
    reaction

-- | Get current reaction setting from environment
--
-- Note: 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s (EE STM) => Execution s React
reaction = head . (++ [_react $ reflect (Proxy :: Proxy s)]) <$> use reactStack


-- | Single command execution
command :: forall a s t. Reifies t (EE STM) => EL SA s a -> Execution t ()
command (EM (Reacting (Just r)) _) = reactStack %= (r :)
command (EM (Reacting Nothing) _)  = reactStack %= drop 1
command (EM (User     (Just u)) _) = usersStack %= (u :)
command (EM (User     Nothing) _)  = usersStack %= drop 1
command c = do
  e <- reflected
  let stv = e^.stm.sudoing
      rtv = e^.stm.running
      log = e^.controls.logger
  xs <- use usersStack
  o  <- op c
  liftIO $ case xs of
    []  -> do
      atomically $ readTVar stv >>= \s -> guard (not s) >> writeTVar rtv True
      log (describe (action c))
      o
      atomically $ writeTVar rtv False
    u:_ -> do
      atomically $ do
        [s, r] <- mapM readTVar [stv, rtv]
        guard (not $ s || r)
        writeTVar stv True
      uid  <- getEffectiveUserID
      uid' <- userID <$> getUserEntryForName u
      setEffectiveUserID uid'
      log (describe (action c))
      o
      setEffectiveUserID uid
      atomically $ writeTVar stv False
 where
  op :: EL SA s a -> Execution t (IO ())
  op (ES _ (S _ _ dst update) _ _) = do
    rstv <- reflected <&> \e -> e^.stm.repos
    return $ do
      updated <- atomically $ do
        rs <- readTVar rstv
        if dst `S.member` rs
          then return True
          else do
            writeTVar rstv $ S.insert dst rs
            return False
      unless updated $ do
        createDirectoryIfMissing True $ dropFileName dst
        update dst
     `E.onException`
      atomically (modifyTVar rstv (S.delete dst))
  op (EA _ (Link src dst) _) = return $ overWriteWith createSymbolicLink src dst
  op (EA _ (Copy src dst) _) = return $ overWriteWith copyFile src dst
  op (EA _ (Template src dst substitute) _) = do
    Templates ts <- view templates <$> reflected
    return $
      overWriteWith (\s d -> toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  op (EA _ (Shell p sc) _) = return $ do
    (_, _, Just er, ph) <- createProcess $
      (shell sc) { cwd = Just p, std_out = CreatePipe, std_err = CreatePipe }
    e <- waitForProcess ph
    case e of
      ExitFailure _ -> T.hGetContents er >>= throwIO . ShellCommandFailure sc
      _ -> return ()
  op _ = return $ return ()

  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst


-- | Queue next task in scheduler
newTask :: forall a s t. Reifies t (EE STM) => Free (EL SA s) a -> Execution t ()
newTask (Pure _) = return ()
newTask t = do
  e <- reflected
  s <- get
  let i = case t of
            Free (EP (SAP { sapToken }) _ _ _) -> sapToken
            Free (ES (SAS { sasToken }) _ _ _) -> sasToken
            _ -> error "???"
  liftIO . atomically . writeTQueue (e^.stm.work) $
    Do i $ do
      runTask e s task t
      atomically (writeTQueue (e^.stm.work) (Stop i))
