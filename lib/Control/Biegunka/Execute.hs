{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Real run interpreter
module Control.Biegunka.Execute
  ( run, dryRun
  , execute, pretend
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List ((\\))
import           Prelude hiding (log)
import           System.IO.Error (tryIOError)

import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Lens hiding (op)
import           Control.Monad.Catch (SomeException, onException, throwM, try)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (get)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (def)
import           Data.Reflection (Reifies)
import qualified Data.Set as S
import qualified Data.Text.IO as T
import qualified System.Directory as D
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.User (getEffectiveUserID, getUserEntryForName, userID, setEffectiveUserID)
import qualified System.Process as P

import Control.Biegunka.Action (copy, applyPatch, verifyAppliedPatch)
import Control.Biegunka.Settings (Settings, Interpreter(..), interpret, local, logger, colors)
import qualified Control.Biegunka.DB as DB
import Control.Biegunka.Execute.Settings
import Control.Biegunka.Execute.Describe (termDescription, runChanges, action, exception, retryCounter)
import Control.Biegunka.Execute.Exception
import Control.Biegunka.Language
import Control.Biegunka.Execute.Schedule (runTask, schedule)
import Control.Biegunka.Script


-- | Real run interpreter
run :: (Run -> Run) -> Interpreter
run e = interpret $ \c (s, as) -> do
  let b = DB.fromScript s
  a <- DB.load c (as^.profiles)
  r <- initializeSTM ((e $ def) & mode.~Real)
  let c' = c & local.~r
  runTask c' def newTask s
  atomically (writeTQueue (c'^.local.sync.work) Stop)
  schedule (c'^.local.sync.work)
  mapM_ (tryIOError . removeFile) (DB.filepaths a \\ DB.filepaths b)
  mapM_ (tryIOError . D.removeDirectoryRecursive) (DB.sources a \\ DB.sources b)
  DB.save c (as^.profiles) b
 where
  removeFile path = do
    file <- D.doesFileExist path
    case file of
      True  -> D.removeFile path
      False -> D.removeDirectoryRecursive path

-- | Real run interpreter
execute :: (Run -> Run) -> Interpreter
execute = run
{-# DEPRECATED execute "Please, use `run'" #-}

-- | Dry run interpreter
dryRun :: Interpreter
dryRun = interpret $ \c (s, as) -> do
  let b = DB.fromScript s
  a <- DB.load c (as^.profiles)
  e <- initializeSTM def
  let c' = c & local.~e
  runTask c' def newTask s
  atomically (writeTQueue (e^.sync.work) Stop)
  schedule (e^.sync.work)
  c'^.logger $ runChanges (c'^.colors) a b

-- | Dry run interpreter
pretend :: Interpreter
pretend = dryRun
{-# DEPRECATED pretend "Please, use `dryRun'" #-}


-- | Run single 'Sources' task
--
-- "Forks" on 'TS'.
-- Note: current thread continues to execute what's inside the
-- task, but all the other stuff is queued for execution in scheduler
task
  :: Reifies t (Settings Execution)
  => Free (Term Annotate Sources) a
  -> Executor t ()
task (Free c@(TS (AS { asToken }) _ b d)) = do
  newTask d
  try (command c) >>= \case
    Left e -> checkRetryCount (getRetries c) e >>= \case
      True  -> task (Free (Pure () <$ c))
      False -> case getReaction c of
        Abortive -> doneWith asToken
        Ignorant -> do
          taskAction b
          doneWith asToken
    Right _ -> do
      taskAction b
      doneWith asToken
task (Free c@(TM _ x)) = do
  command c
  task x
task (Pure _) = return ()

-- | Run single 'Actions' task
taskAction
  :: Reifies t (Settings Execution)
  => Free (Term Annotate Actions) a
  -> Executor t ()
taskAction a@(Free c@(TA _ _ x)) =
  try (command c) >>= \case
    Left e -> checkRetryCount (getRetries c) e >>= \case
      True  -> taskAction a
      False -> case getReaction c of
        Abortive -> return ()
        Ignorant -> taskAction x
    Right _ -> taskAction x
taskAction (Free c@(TM _ x)) = do
  command c
  taskAction x
taskAction (Pure _) = return ()


-- | Get response from task failure processing
--
-- Possible responses: retry command execution or ignore failure or abort task
checkRetryCount
  :: forall s. Reifies s (Settings Execution)
  => Int
  -> SomeException
  -> Executor s Bool
checkRetryCount maximumRetries exc = do
  log <- env^!acts.logger
  scm <- env^!acts.colors
  liftIO . log . termDescription $ exception scm exc
  doneRetries <- retryCount <%= (+1)
  if doneRetries <= maximumRetries then do
    liftIO . log . termDescription $ retryCounter scm doneRetries maximumRetries
    return True
  else do
    retryCount .= 0
    return False


-- | Single command execution
command :: Reifies t (Settings Execution)
        => Term Annotate s a
        -> Executor t ()
command (TM (Wait ts) _)           = do
  ts' <- env^!acts.local.sync.tasks
  liftIO . atomically $ do
    ts'' <- readTVar ts'
    unless (ts `S.isSubsetOf` ts'')
      retry
command c = do
  stv <- env^!acts.local.sync.sudoing
  rtv <- env^!acts.local.sync.running
  log <- env^!acts.logger
  scm <- env^!acts.colors
  op  <- env^!acts.local.runs.mode.act (\case Dry -> termEmptyOperation c; Real -> termOperation c)
  liftIO $ case getUser c of
    Nothing  -> do
      atomically $ readTVar stv >>= \s -> guard (not s) >> writeTVar rtv True
      log (termDescription (action scm c))
      op
      atomically $ writeTVar rtv False
    Just user -> do
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

termOperation :: Reifies t (Settings Execution)
              => Term Annotate s a
              -> Executor t (IO ())
termOperation term = case term of
  TS _ (Source _ _ dst update) _ _ -> do
    rstv <- env^!acts.local.sync.repos
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
    Templates ts <- env^!acts.local.runs.templates
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

termEmptyOperation :: Reifies t (Settings Execution)
                   => Term Annotate s a
                   -> Executor t (IO ())
termEmptyOperation _ = return (return ())

-- | Queue next task in scheduler
newTask :: forall a t. Reifies t (Settings Execution)
        => Free (Term Annotate Sources) a
        -> Executor t ()
newTask (Pure _) = return ()
newTask t = do
  e <- env
  s <- get
  liftIO . atomically . writeTQueue (e^.local.sync.work) $
    Do $ do
      runTask e s task t
      atomically (writeTQueue (e^.local.sync.work) Stop)

-- | Tell execution process that you're done with task
doneWith :: Reifies t (Settings Execution) => Int -> Executor t ()
doneWith n = do
  ts <- env^!acts.local.sync.tasks
  liftIO . atomically $
    modifyTVar ts (S.insert n)


-- | Get retries maximum associated with term
getRetries :: Term Annotate s a -> Int
getRetries (TS (AS { asMaxRetries }) _ _ _) = asMaxRetries
getRetries (TA (AA { aaMaxRetries }) _ _) = aaMaxRetries
getRetries (TM _ _) = 0

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
