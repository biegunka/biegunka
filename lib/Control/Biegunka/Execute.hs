{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Real run interpreter
module Control.Biegunka.Execute
  ( run, dryRun
  ) where

import           Control.Applicative
import           Control.Monad
import           Prelude hiding (log)
import           System.IO.Error (tryIOError)

import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Lens hiding (op)
import           Control.Monad.Catch
  (SomeException, bracket, onException, throwM, try)
import           Control.Monad.Free (Free(..))
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (Default(..))
import           Data.Reflection (Reifies)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified System.Directory as D
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.User (getEffectiveUserID, getUserEntryForName, userID, setEffectiveUserID)
import qualified System.Process as P

import           Control.Biegunka.Action (copy, applyPatch, verifyAppliedPatch)
import           Control.Biegunka.Settings
  (Settings, Templates(..), templates, local, logger, colors)
import qualified Control.Biegunka.Groups as Groups
import           Control.Biegunka.Execute.Settings
import           Control.Biegunka.Execute.Describe
  (termDescription, runChanges, action, exception, removal, retryCounter)
import           Control.Biegunka.Execute.Exception
import           Control.Biegunka.Language
import           Control.Biegunka.Biegunka
  (Interpreter(..), interpret)
import           Control.Biegunka.Execute.Schedule (runTask, schedule)
import           Control.Biegunka.Script


-- | Real run interpreter
run :: Interpreter
run = interpret interpreting where
  interpreting settings s = do
    let db' = Groups.fromScript s
    bracket (Groups.open settings) Groups.close $ \db -> do
      r <- initializeSTM
      let settings' = settings & local .~ r
      runTask settings' (newTask termOperation) s
      atomically (writeTQueue (settings'^.local.work) Stop)
      schedule (settings'^.local.work)
      mapM_ (catched remove)          (Groups.diff Groups.files   (db^.Groups.these) db')
      mapM_ (catched removeDirectory) (Groups.diff Groups.sources (db^.Groups.these) db')
      Groups.commit (db & Groups.these .~ db')
   where
    remove path = do
      file <- D.doesFileExist path
      case file of
        True  -> do
          (settings^.logger) (removal path)
          D.removeFile path
        False -> D.removeDirectoryRecursive path

    removeDirectory path = do
      directory <- D.doesDirectoryExist path
      case directory of
        True  -> do
          (settings^.logger) (removal path)
          D.removeDirectoryRecursive path
        False -> return ()

    catched io = tryIOError . io

-- | Dry run interpreter
dryRun :: Interpreter
dryRun = interpret $ \settings s -> do
  let db' = Groups.fromScript s
  bracket (Groups.open settings) Groups.close $ \db -> do
    r <- initializeSTM
    let settings' = settings & local .~ r
    runTask settings' (newTask termEmptyOperation) s
    atomically (writeTQueue (settings'^.local.work) Stop)
    schedule (settings'^.local.work)
    settings'^.logger $ runChanges (settings'^.colors) db db'


-- | Run single 'Sources' task
--
-- "Forks" on 'TS'.
-- Note: current thread continues to execute what's inside the
-- task, but all the other stuff is queued for execution in scheduler
task
  :: forall t. Reifies t (Settings Execution)
  => (forall s b t'. Reifies t' (Settings Execution) => Term Annotate s b -> Executor t' (IO ()))
  -> Retry
  -> Free (Term Annotate Sources) ()
  -> Executor t ()
task f = go
 where
  go
    :: Reifies t (Settings Execution)
    => Retry
    -> Free (Term Annotate Sources) ()
    -> Executor t ()
  go retries (Free c@(TS (AS { asToken }) _ b d)) = do
    newTask f d
    try (command f c) >>= \case
      Left e -> checkRetryCount retries (getRetries c) e >>= \case
        True  -> go (incr retries) (Free (Pure () <$ c))
        False -> case getReaction c of
          Abortive -> doneWith asToken
          Ignorant -> do
            taskAction f def b
            doneWith asToken
      Right _ -> do
        taskAction f def b
        doneWith asToken
  go _ (Free c@(TM _ x)) = do
    command f c
    go def x
  go _ (Pure _) = return ()

-- | Run single 'Actions' task
taskAction
  :: forall t. Reifies t (Settings Execution)
  => (forall a s. Term Annotate s a -> Executor t (IO ()))
  -> Retry
  -> Free (Term Annotate Actions) ()
  -> Executor t ()
taskAction f = go
 where
  go
    :: Reifies t (Settings Execution)
    => Retry
    -> Free (Term Annotate Actions) a
    -> Executor t ()
  go retries a@(Free c@(TA _ _ x)) =
    try (command f c) >>= \case
      Left e -> checkRetryCount retries (getRetries c) e >>= \case
        True  -> go (incr retries) a
        False -> case getReaction c of
          Abortive -> return ()
          Ignorant -> go def x
      Right _ -> go def x
  go _ (Free c@(TM _ x)) = do
    command f c
    go def x
  go _ (Pure _) = return ()


-- | Get response from task failure processing
--
-- Possible responses: retry command execution or ignore failure or abort task
checkRetryCount
  :: forall s. Reifies s (Settings Execution)
  => Retry
  -> Retry
  -> SomeException
  -> Executor s Bool
checkRetryCount doneRetries maximumRetries exc = do
  log <- env^!acts.logger
  scm <- env^!acts.colors
  liftIO . log . termDescription $ exception scm exc
  if doneRetries < maximumRetries then do
    liftIO . log . termDescription $ retryCounter scm (unRetry (incr doneRetries)) (unRetry maximumRetries)
    return True
  else do
    return False


-- | Single command execution
command
  :: Reifies t (Settings Execution)
  => (Term Annotate s a -> Executor t (IO ()))
  -> Term Annotate s a
  -> Executor t ()
command _ (TM (Wait ts) _) = do
  ts' <- env^!acts.local.tasks
  liftIO . atomically $ do
    ts'' <- readTVar ts'
    unless (ts `S.isSubsetOf` ts'')
      retry
command f c = do
  stv <- env^!acts.local.sudoing
  rtv <- env^!acts.local.running
  log <- env^!acts.logger
  scm <- env^!acts.colors
  op  <- f c
  liftIO $ case getUser c of
    Nothing  -> do
      atomically $ readTVar stv >>= \s -> guard (not s) >> writeTVar rtv True
      log (termDescription (action scm c))
      op
      atomically $ writeTVar rtv False
    Just (UserW user) -> do
      atomically $ do
        [s, r] <- mapM readTVar [stv, rtv]
        guard (not $ s || r)
        writeTVar stv True
      uid  <- getEffectiveUserID
      uid' <- getUID user
      log (termDescription (action scm c))
      setEffectiveUserID uid'
      op
      setEffectiveUserID uid
      atomically $ writeTVar stv False
 where
  getUID (UserID i)   = return i
  getUID (Username n) = userID <$> getUserEntryForName n

termOperation
  :: Reifies t (Settings Execution)
  => Term Annotate s a
  -> Executor t (IO ())
termOperation term = case term of
  TS _ (Source _ _ dst update) _ _ -> do
    rstv <- env^!acts.local.repos
    return $ do
      updated <- atomically $ do
        rs <- readTVar rstv
        if dst `S.member` rs
          then return True
          else do
            writeTVar rstv $ S.insert dst rs
            return False
      unless updated $ do
        D.createDirectoryIfMissing True $ dropFileName dst
        update dst
     `onException`
      atomically (modifyTVar rstv (S.delete dst))
  TA _ (Link src dst) _ -> return $ overWriteWith createSymbolicLink src dst
  TA _ (Copy src dst spec) _ -> return $ do
    try (D.removeDirectoryRecursive dst) :: IO (Either IOError ())
    D.createDirectoryIfMissing True $ dropFileName dst
    copy src dst spec
  TA _ (Template src dst substitute) _ -> do
    Templates ts <- env^!acts.templates
    return $
      overWriteWith (\s d -> T.writeFile d . substitute ts =<< T.readFile s) src dst
  TA _ (Command p spec) _ -> return $ do
    (_, _, Just errors, ph) <- P.createProcess $
      P.CreateProcess
        { P.cmdspec      = spec
        , P.cwd          = Just p
        , P.env          = Nothing
        , P.std_in       = P.Inherit
        , P.std_out      = P.CreatePipe
        , P.std_err      = P.CreatePipe
        , P.close_fds    = False
        , P.create_group = False
        }
    e <- P.waitForProcess ph
    e `onFailure` \status ->
      T.hGetContents errors >>= throwM . ShellException spec status
  TA _ (Patch patch root spec) _ -> return $ do
    verified <- verifyAppliedPatch patch root spec
    unless verified $
      applyPatch patch root spec
  TM _ _ -> return $ return ()
 where
  overWriteWith g src dst = do
    D.createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst

termEmptyOperation
  :: Reifies t (Settings Execution)
  => Term Annotate s a
  -> Executor t (IO ())
termEmptyOperation _ = return (return ())

-- | Queue next task in scheduler
newTask
  :: forall t. Reifies t (Settings Execution)
  => (forall s b t'. Reifies t' (Settings Execution) => Term Annotate s b -> Executor t' (IO ()))
  -> Free (Term Annotate Sources) ()
  -> Executor t ()
newTask _ (Pure _) = return ()
newTask f t = do
  e <- env
  liftIO . atomically . writeTQueue (e^.local.work) $
    Do $ do
      runTask e (task f def) t
      atomically (writeTQueue (e^.local.work) Stop)

-- | Tell execution process that you're done with task
doneWith :: Reifies t (Settings Execution) => Int -> Executor t ()
doneWith n = do
  ts <- env^!acts.local.tasks
  liftIO . atomically $
    modifyTVar ts (S.insert n)


-- | Get retries maximum associated with term
getRetries :: Term Annotate s a -> Retry
getRetries (TS (AS { asMaxRetries }) _ _ _) = asMaxRetries
getRetries (TA (AA { aaMaxRetries }) _ _) = aaMaxRetries
getRetries (TM _ _) = def

-- | Get user associated with term
getUser :: Term Annotate s a -> Maybe UserW
getUser (TS (AS { asUser }) _ _ _) = asUser
getUser (TA (AA { aaUser }) _ _) = aaUser
getUser (TM _ _) = Nothing

-- | Get reaction associated with term
getReaction :: Term Annotate s a -> React
getReaction (TS (AS { asReaction }) _ _ _) = asReaction
getReaction (TA (AA { aaReaction }) _ _) = aaReaction
getReaction (TM _ _) = Ignorant
