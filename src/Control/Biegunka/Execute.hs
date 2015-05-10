{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
-- | Real run interpreter
module Control.Biegunka.Execute
  ( run, dryRun
  ) where

import           Control.Applicative
import           Control.Monad
import           Prelude hiding (log, null)
import           System.IO.Error (tryIOError)

import           Control.Concurrent (forkFinally)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Lens hiding (op)
import           Control.Lens.Extras (is)
import           Control.Monad.Catch
  (SomeException, bracket, bracket_, onException, throwM, try)
import           Control.Monad.Free (Free(..))
import           Control.Monad.Reader (runReaderT, ask)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default.Class (Default(..))
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified System.Directory as D
import           System.FilePath (dropFileName)
import           System.Environment (getEnvironment)
import qualified System.Posix as Posix
import qualified System.Process as P

import           Control.Biegunka.Action (copy, applyPatch, verifyAppliedPatch)
import qualified Control.Biegunka.Log as Log
import           Control.Biegunka.Settings
  (Settings, Templates(..), templates, local, logger, colors, Mode(..), mode)
import qualified Control.Biegunka.Groups as Groups
import           Control.Biegunka.Execute.Settings
import           Control.Biegunka.Execute.Describe
  (termDescription, runChanges, action, exception, removal, retryCounter)
import           Control.Biegunka.Execute.Exception
import           Control.Biegunka.Language
import           Control.Biegunka.Biegunka (Interpreter, interpretOptimistically)
import qualified Control.Biegunka.Execute.Watcher as Watcher
import           Control.Biegunka.Script

{-# ANN module "HLint: ignore Use if" #-}


-- | Real run interpreter
run :: Interpreter
run = interpretOptimistically go where
  go settings s = do
    let db' = Groups.fromScript s
    bracket (Groups.open settings) Groups.close $ \db -> do
      mapM_ (safely remove)          (Groups.diff Groups.files   (view Groups.these db) db')
      mapM_ (safely removeDirectory) (Groups.diff Groups.sources (view Groups.these db) db')
      bracket Posix.getEffectiveUserID Posix.setEffectiveUserID $ \_ ->
        bracket Posix.getEffectiveGroupID Posix.setEffectiveGroupID $ \_ ->
          bracket Watcher.new Watcher.wait $ \watcher -> do
            r <- initializeSTM watcher
            runTask (set local r settings) (task (views mode io settings) def) s
      Groups.commit (db & Groups.these .~ db')
   where
    io Offline = runIOOffline
    io Online  = runIOOnline

    remove path = do
      file <- D.doesFileExist path
      case file of
        True  -> do
          Log.write (view logger settings) $
            Log.plain (removal path)
          D.removeFile path
        False -> D.removeDirectoryRecursive path

    removeDirectory path = do
      directory <- D.doesDirectoryExist path
      case directory of
        True  -> do
          Log.write (view logger settings) $
            Log.plain (removal path)
          D.removeDirectoryRecursive path
        False -> return ()

    safely doThings = tryIOError . doThings

-- | Dry run interpreter
dryRun :: Interpreter
dryRun = interpretOptimistically $ \settings s -> do
  let db' = Groups.fromScript s
  bracket (Groups.open settings) Groups.close $ \db -> do
    bracket Watcher.new Watcher.wait $ \watcher -> do
      r <- initializeSTM watcher
      runTask (set local r settings) (task runPure def) s
    Log.write (view logger settings) $
      Log.plain (runChanges (view colors settings) db db')


-- | Prepares environment to run task with given execution routine
runTask
  :: forall s m. MonadIO m
  => Settings Execution
  -> (Free (Term Annotate s) () -> Executor ())
  -> Free (Term Annotate s) () -- ^ Task contents
  -> m ()
runTask e f s = do
  Watcher.register (view watch e)
  liftIO $ forkFinally (runReaderT (f s) e) (\_ -> Watcher.unregister (view watch e))
  return ()


-- | Run single 'Sources' task
--
-- "Forks" on 'TS'.
-- Note: current thread continues to execute what's inside the
-- task, but all the other stuff is queued for execution in scheduler
task
  :: (forall a s. Term Annotate s a -> Executor (IO ()))
  -> Retry
  -> Free (Term Annotate 'Sources) ()
  -> Executor ()
task f = go
 where
  go retries (Free c@(TS _ _ _ t@(Free _))) = do
    e <- ask
    runTask e (task f def) t
    go retries (Free (Pure () <$ c))
  go retries (Free c@(TS (AS { asToken }) _ b (Pure _))) =
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
  :: (forall a s. Term Annotate s a -> Executor (IO ()))
  -> Retry
  -> Free (Term Annotate 'Actions) ()
  -> Executor ()
taskAction f = go
 where
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
checkRetryCount :: Retry -> Retry -> SomeException -> Executor Bool
checkRetryCount doneRetries maximumRetries exc = do
  log <- view logger
  scm <- view colors
  liftIO $ do
    Log.write log $
      Log.exception (termDescription (exception scm exc))
    if doneRetries < maximumRetries then do
      Log.write log $
        Log.exception (termDescription $
          retryCounter scm (unRetry (incr doneRetries)) (unRetry maximumRetries))
      return True
    else
      return False


-- | Single command execution
command :: (Term Annotate s a -> Executor (IO ())) -> Term Annotate s a -> Executor ()
command _ (TM (Wait waits) _) = do
  watcher <- view watch
  Watcher.waitDone watcher waits
command getIO term = do
  users <- view user
  io    <- getIO term
  liftIO $ case getUser term of
    Nothing  ->
      io
    Just u -> do
      -- I really hope that stuff does not change
      -- while biegunka run is in progress
      gid <- userGroupID u
      uid <- userID u
      bracket_ (acquire users uid) (release users uid) $ do
        Posix.setEffectiveGroupID gid
        Posix.setEffectiveUserID uid
        io
 where
  acquire users uid = atomically $ do
    -- So, first get current user/count
    mu <- readTVar users
    case view (at uid) mu of
      -- If *this* user is not inside yet
      Nothing
          -- and there is no *current* user, just let him in
          -- and set counter to 1
        | is _Empty mu -> writeTVar users (mu & at uid ?~ 1)
          -- and there is a *current* user, retry
          -- until he leaves
        | otherwise -> retry
      -- If *this* user is inside, increment the counter and
      -- let him in
      Just _ ->
        writeTVar users (mu & ix uid +~ 1)

  release users uid = atomically $
    -- On leave, decrement counter
    -- If counter approaches zero, then current user left
    modifyTVar users (at uid . non 0 -~ 1)

userID :: User -> IO Posix.UserID
userID (UserID i)   = return i
userID (Username n) = Posix.userID <$> Posix.getUserEntryForName n

userGroupID :: User -> IO Posix.GroupID
userGroupID (UserID i)   = Posix.userGroupID <$> Posix.getUserEntryForID i
userGroupID (Username n) = Posix.userGroupID <$> Posix.getUserEntryForName n

runIOOnline :: Term Annotate s a -> Executor (IO ())
runIOOnline term = do
  log <- view logger
  scm <- view colors
  io  <- ioOnline term
  let message = Log.write log (Log.plain (termDescription (action scm term)))
  return (message *> io)

ioOnline :: Term Annotate s a -> Executor (IO ())
ioOnline term = case term of
  TS _ (Source _ _ dst update) _ _ -> do
    rstv <- view repos
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

  TA _ (Link src dst) _ -> return $ overWriteWith Posix.createSymbolicLink src dst

  TA _ (Copy src dst spec) _ -> return $ do
    try (D.removeDirectoryRecursive dst) :: IO (Either IOError ())
    D.createDirectoryIfMissing True $ dropFileName dst
    copy src dst spec

  TA _ (Template src dst substitute) _ -> do
    Templates ts <- view templates
    return $
      overWriteWith (\s d -> T.writeFile d . substitute ts =<< T.readFile s) src dst

  TA ann (Command p spec) _ -> return $ do
    env <- getEnvironment
    (_, _, Just errors, ph) <- P.createProcess
      P.CreateProcess
        { P.cmdspec       = spec
        , P.cwd           = Just p
        , P.env =
            Just ( ("RUN_ROOT",    view runRoot    ann)
                 : ("SOURCE_ROOT", view sourceRoot ann)
                 : env)
        , P.std_in        = P.Inherit
        , P.std_out       = P.CreatePipe
        , P.std_err       = P.CreatePipe
        , P.close_fds     = False
        , P.create_group  = False
        , P.delegate_ctlc = False
        }
    e <- P.waitForProcess ph
    e `onFailure` \status ->
      T.hGetContents errors >>= throwM . ShellException spec status

  TA _ (Patch patch file spec) _ -> return $ do
    verified <- verifyAppliedPatch patch file spec
    unless verified $
      applyPatch patch file spec

  TM _ _ -> return $ return ()
 where
  overWriteWith g src dst = do
    D.createDirectoryIfMissing True $ dropFileName dst
    tryIOError (Posix.removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst

runIOOffline :: Term Annotate s a -> Executor (IO ())
runIOOffline t@(TS {}) = runPure t
runIOOffline t         = runIOOnline t

runPure :: Applicative m => Term Annotate s a -> Executor (m ())
runPure _ = pure (pure ())

-- | Tell execution process that you're done with task
doneWith :: Token -> Executor ()
doneWith tok = do
  watcher <- view watch
  Watcher.done watcher tok


-- | Get retries maximum associated with term
getRetries :: Term Annotate s a -> Retry
getRetries (TS (AS { asMaxRetries }) _ _ _) = asMaxRetries
getRetries (TA (AA { aaMaxRetries }) _ _) = aaMaxRetries
getRetries (TM _ _) = def

-- | Get user associated with term
getUser :: Term Annotate s a -> Maybe User
getUser (TS (AS { asUser }) _ _ _) = asUser
getUser (TA (AA { aaUser }) _ _) = aaUser
getUser (TM _ _) = Nothing

-- | Get reaction associated with term
getReaction :: Term Annotate s a -> React
getReaction (TS (AS { asReaction }) _ _ _) = asReaction
getReaction (TA (AA { aaReaction }) _ _) = aaReaction
getReaction (TM _ _) = Ignorant
