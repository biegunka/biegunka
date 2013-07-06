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
import           Data.Copointed (copoint)
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

import Biegunka.Control (Controls, Interpreter(..), interpret, interpreter, logger, colors)
import Biegunka.DB
import Biegunka.Execute.Control
import Biegunka.Execute.Describe (termDescription, runChanges, action, exception, retryCounter)
import Biegunka.Execute.Exception
import Biegunka.Language (Term(..), Action(..), Source(..), Modifier(..), React(..))
import Biegunka.Execute.Schedule (runTask, schedule)
import Biegunka.Script


-- | Real run interpreter
run :: (EE () -> EE ()) -> Interpreter
run ee = interpret $ \c s -> do
  let b = construct s
  a <- load c s
  (set mode Real -> ee') <- initializeSTM (ee def)
  let c' = set interpreter ee' c
  dropPriviledges ee'
  runTask c' def newTask s
  atomically (writeTQueue (c'^.interpreter.stm.work) (Stop 0)) -- Can we assume script starts from id 0?
  schedule (c'^.interpreter.stm.work)
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

-- | Dru run interpreter
dryRun :: Interpreter
dryRun = interpret $ \c s -> do
  let b = construct s
  a <- load c s
  ee' <- initializeSTM def
  let c' = set interpreter ee' c
  runTask c' def newTask s
  atomically (writeTQueue (ee'^.stm.work) (Stop 0)) -- Can we assume script starts from id 0?
  schedule (ee'^.stm.work)
  c'^.logger $ runChanges (c'^.colors) a b

-- | Dru run interpreter
pretend :: Interpreter
pretend = dryRun
{-# DEPRECATED pretend "Please, use `dryRun'" #-}


-- | Run single task command by command
--
-- "Forks" on 'Task' 'True'.
-- Note: current thread continues to execute what's inside task, but all the other stuff is queued
--
-- Complexity comes from forking and responding to errors. Otherwise that is dead simple function
task :: Reifies t (Controls (EE STM)) => Free (Term Annotate s) a -> Executor t ()
task (Free (TP _ _ b d)) = do
  newTask d
  task b
task (Free c@(TS _ _ b d)) = do
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
retry :: forall s. Reifies s (Controls (EE STM)) => SomeException -> Executor s React
retry exc = do
  e <- reflected
  let log = e^.logger
      scm = e^.colors
  liftIO . log . termDescription $ exception scm exc
  rc <- retryCount <<%= (+1)
  if rc < (reflect (Proxy :: Proxy s))^.interpreter.retries then do
    liftIO . log . termDescription $ retryCounter scm (rc + 1)
    return Retry
  else do
    retryCount .= 0
    reaction

-- | Get current reaction setting from environment
--
-- Note: 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s (Controls (EE STM)) => Executor s React
reaction = head . (++ [(reflect (Proxy :: Proxy s))^.interpreter.react]) <$> use reactStack


-- | Single command execution
command :: Reifies t (Controls (EE STM)) => Term Annotate s a -> Executor t ()
command (TM (Reacting (Just r)) _) = reactStack %= (r :)
command (TM (Reacting Nothing) _)  = reactStack %= drop 1
command (TM (User     (Just u)) _) = usersStack %= (u :)
command (TM (User     Nothing) _)  = usersStack %= drop 1
command c = do
  e <- reflected
  let stv = e^.interpreter.stm.sudoing
      rtv = e^.interpreter.stm.running
      log = e^.logger
      scm = e^.colors
  xs <- use usersStack
  op  <- case e^.interpreter.mode of
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

termOperation :: Reifies t (Controls (EE STM)) => Term Annotate s a -> Executor t (IO ())
termOperation term = case term of
  TS _ (Source _ _ dst update) _ _ -> do
    rstv <- reflected <&> \e -> e^.interpreter.stm.repos
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
  TA _ (Copy src dst) _ -> return $ overWriteWith copyFile src dst
  TA _ (Template src dst substitute) _ -> do
    Templates ts <- view (interpreter.templates) <$> reflected
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
      ExitFailure _ -> T.hGetContents er >>= throwIO . ShellCommandFailure sp
      _ -> return ()
  _ -> return $ return ()
 where
  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst

termEmptyOperation :: Reifies t (Controls (EE STM)) => Term Annotate s a -> Executor t (IO ())
termEmptyOperation _ = return (return ())

-- | Queue next task in scheduler
newTask :: forall a s t. Reifies t (Controls (EE STM)) => Free (Term Annotate s) a -> Executor t ()
newTask (Pure _) = return ()
newTask t = do
  e <- reflected
  s <- get
  let i = case t of
            Free (TP (AP { apToken }) _ _ _) -> apToken
            Free (TS (AS { asToken }) _ _ _) -> asToken
            _ -> error "???"
  liftIO . atomically . writeTQueue (e^.interpreter.stm.work) $
    Do i $ do
      runTask e s task t
      atomically (writeTQueue (e^.interpreter.stm.work) (Stop i))
