{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- | Real run interpreter
module Biegunka.Execute
  ( run, dryRun
  , execute, pretend
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, IOException, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.Foldable (traverse_)
import           Data.List ((\\))
import           Prelude hiding (log)
import           System.Exit (ExitCode(..))
import           System.IO.Error (tryIOError)

import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Lens hiding (op)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Copointed (copoint)
import           Data.Default (def)
import           Data.Proxy
import           Data.Reflection
import           Data.Functor.Trans.Tagged
import qualified Data.Set as S
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (removeDirectoryRecursive, removeFile, createDirectoryIfMissing)
import           System.FilePath (dropFileName)
import           System.Posix.Files (createSymbolicLink, removeLink)
import           System.Posix.Env (getEnv)
import           System.Posix.User (getEffectiveUserID, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process

import Biegunka.Action (copy, applyPatch, verifyAppliedPatch)
import Biegunka.Control (Settings, Interpreter(..), interpret, local, logger, colors)
import qualified Biegunka.DB as DB
import Biegunka.Execute.Control
import Biegunka.Execute.Describe (termDescription, runChanges, action, exception, retryCounter)
import Biegunka.Execute.Exception
import Biegunka.Language (Term(..), Action(..), Source(..), Modifier(..), React(..))
import Biegunka.Execute.Schedule (runTask, schedule)
import Biegunka.Script


-- | Real run interpreter
run :: (Run -> Run) -> Interpreter
run e = interpret $ \c (s, as) -> do
  let b = DB.fromScript s
  a <- DB.load c (as^.profiles)
  r <- initializeSTM ((e $ def) & mode.~Real)
  let c' = c & local.~r
  dropPriviledges r
  runTask c' def newTask s
  atomically (writeTQueue (c'^.local.sync.work) Stop)
  schedule (c'^.local.sync.work)
  mapM_ (tryIOError . removeFile) (DB.filepaths a \\ DB.filepaths b)
  mapM_ (tryIOError . removeDirectoryRecursive) (DB.sources a \\ DB.sources b)
  DB.save c (as^.profiles) b

-- | Real run interpreter
execute :: (Run -> Run) -> Interpreter
execute = run
{-# DEPRECATED execute "Please, use `run'" #-}

dropPriviledges :: Execution -> IO ()
dropPriviledges e =
  case e^.runs.priviledges of
    Drop     -> getEnv "SUDO_USER" >>= traverse_ setUser
    Preserve -> return ()
 where
  setUser n = getUserEntryForName n >>= setEffectiveUserID . userID

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


-- | Run single task command by command
--
-- "Forks" on 'Task' 'True'.
-- Note: current thread continues to execute what's inside task, but all the other stuff is queued
--
-- Complexity comes from forking and responding to errors. Otherwise that is dead simple function
task :: Reifies t (Settings Execution)
     => Free (Term Annotate s) a
     -> Executor t ()
task (Free c@(TS (AS { asToken }) _ b d)) = do
  newTask d
  try (command c) >>= \e -> case e of
    Left e' -> checkRetryCountAndReact e' >>= \r -> case r of
      Retry    -> task (Free (Pure () <$ c))
      Abortive -> do
        doneWith asToken
      Ignorant -> do
        task b
        doneWith asToken
    Right _ -> do
      task b
      doneWith asToken
task a@(Free c) =
  try (command c) >>= \e -> case e of
    Left e' -> checkRetryCountAndReact e' >>= \r -> case r of
      Retry    -> task a
      Abortive -> return ()
      Ignorant -> task (copoint c)
    Right _ -> task (copoint c)
task (Pure _) = return ()


-- | If only I could come up with MonadBaseControl instance for 'Executor'
try :: Exception e => Executor s a -> Executor s (Either e a)
try (TagT ex) = do
  eeas <- liftIO . E.try . runStateT ex =<< get
  case eeas of
    Left e       ->          return (Left e)
    Right (a, s) -> put s >> return (Right a)

-- | Get response from task failure processing
--
-- Possible responses: retry command execution or ignore failure or abort task
checkRetryCountAndReact
  :: forall s. Reifies s (Settings Execution)
  => SomeException
  -> Executor s React
checkRetryCountAndReact exc = do
  e <- reflected
  let log = e^.logger
      scm = e^.colors
  liftIO . log . termDescription $ exception scm exc
  rc <- retryCount <<%= (+1)
  if rc < (reflect (Proxy :: Proxy s))^.local.runs.retries then do
    liftIO . log . termDescription $ retryCounter scm (rc + 1)
    return Retry
  else do
    retryCount .= 0
    reaction

-- | Get current reaction setting from environment
--
-- Note: 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s (Settings Execution) => Executor s React
reaction = head . (++ [(reflect (Proxy :: Proxy s))^.local.runs.react]) <$> use reactStack


-- | Single command execution
command :: Reifies t (Settings Execution)
        => Term Annotate s a
        -> Executor t ()
command (TM (Reacting (Just r)) _) = reactStack %= (r :)
command (TM (Reacting Nothing) _)  = reactStack %= drop 1
command (TM (User     (Just u)) _) = usersStack %= (u :)
command (TM (User     Nothing) _)  = usersStack %= drop 1
command (TM (Wait ts) _)           = do
  ts' <- reflected <&> \e -> e^.local.sync.tasks
  liftIO . atomically $ do
    ts'' <- readTVar ts'
    unless (ts `S.isSubsetOf` ts'')
      retry
command c = do
  e <- reflected
  let stv = e^.local.sync.sudoing
      rtv = e^.local.sync.running
      log = e^.logger
      scm = e^.colors
  xs <- use usersStack
  op  <- case e^.local.runs.mode of
          Dry  -> termEmptyOperation c
          Real -> termOperation c
  liftIO $ case xs of
    []  -> do
      atomically $ readTVar stv >>= \s -> guard (not s) >> writeTVar rtv True
      log (termDescription (action scm c))
      op
      atomically $ writeTVar rtv False
    u:_ -> do
      atomically $ do
        [s, r] <- mapM readTVar [stv, rtv]
        guard (not $ s || r)
        writeTVar stv True
      uid  <- getEffectiveUserID
      uid' <- userID <$> getUserEntryForName u
      setEffectiveUserID uid'
      log (termDescription (action scm c))
      op
      setEffectiveUserID uid
      atomically $ writeTVar stv False

termOperation :: Reifies t (Settings Execution)
              => Term Annotate s a
              -> Executor t (IO ())
termOperation term = case term of
  TS _ (Source _ _ dst update) _ _ -> do
    rstv <- reflected <&> \e -> e^.local.sync.repos
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
  TA _ (Link src dst) _ -> return $ overWriteWith createSymbolicLink src dst
  TA _ (Copy src dst spec) _ -> return $ do
    E.try (removeDirectoryRecursive dst) :: IO (Either IOException ())
    createDirectoryIfMissing True $ dropFileName dst
    copy src dst spec
  TA _ (Template src dst substitute) _ -> do
    Templates ts <- reflected <&> \e -> e^.local.runs.templates
    return $
      overWriteWith (\s d -> toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  TA _ (Command p sp) _ -> return $ do
    (_, _, Just er, ph) <- createProcess $
      CreateProcess
        { cmdspec      = sp
        , cwd          = Just p
        , env          = Nothing
        , std_in       = Inherit
        , std_out      = CreatePipe
        , std_err      = CreatePipe
        , close_fds    = False
        , create_group = False
        }
    e <- waitForProcess ph
    case e of
      ExitFailure _ -> T.hGetContents er >>= throwIO . ShellException sp
      _ -> return ()
  TA _ (Patch patch root spec) _ -> return $ do
    verified <- verifyAppliedPatch patch root spec
    unless verified $
      applyPatch patch root spec
  TM _ _ -> return $ return ()
 where
  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst

termEmptyOperation :: Reifies t (Settings Execution)
                   => Term Annotate s a
                   -> Executor t (IO ())
termEmptyOperation _ = return (return ())

-- | Queue next task in scheduler
newTask :: forall a s t. Reifies t (Settings Execution)
        => Free (Term Annotate s) a
        -> Executor t ()
newTask (Pure _) = return ()
newTask t = do
  e <- reflected
  s <- get
  liftIO . atomically . writeTQueue (e^.local.sync.work) $
    Do $ do
      runTask e s task t
      atomically (writeTQueue (e^.local.sync.work) Stop)

-- | Tell execution process that you're done with task
doneWith :: Reifies t (Settings Execution) => Int -> Executor t ()
doneWith n = do
  ts' <- reflected <&> \e -> e^.local.sync.tasks
  liftIO . atomically $
    modifyTVar ts' (S.insert n)
