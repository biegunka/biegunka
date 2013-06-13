{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Execute (execute) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.Foldable (for_, traverse_)
import           Data.List ((\\))
import           Data.Monoid (mempty)
import           Prelude hiding (log)
import           System.Exit (ExitCode(..))
import           System.IO.Error (tryIOError)

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically)
import           Control.Lens hiding (op)
import           Control.Monad.Free (Free(..))
import           Control.Monad.State (evalStateT, runStateT, get, put)
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

import Biegunka.Control (Interpreter(..), logger)
import Biegunka.DB
import Biegunka.Execute.Control
import Biegunka.Execute.Exception
import Biegunka.Execute.Describe (describe, action, exception, retryCounter)
import Biegunka.Language (EL(..), A(..), S(..), W(..), React(..), peek)
import Biegunka.Script
import Biegunka.Transform (fromEL, simplified)


-- | Execute Interpreter
--
-- Biegunka workhorse. Does useful work: copy and links files, compiles stuff, anything else
--
-- Supports setting execution options via its first argument
--
-- It's generally advised to use 'pretend' before 'execute': that way you can catch some
-- bugs in your script before devastation is done.
execute :: (EE -> EE) -> Interpreter
execute (($ def) -> e) = I $ \c s -> do
  let s' = simplified $ fromEL s
      b = construct s'
  a <- load c s'
  e' <- initTVars e
  when (e' ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  atomically $ writeTQueue (e'^.stm.work)
    (Do $ runTask e' { _controls = c } def s >> atomically (writeTQueue (e'^.stm.work) Stop))
  schedule (e'^.stm.work)
  mapM_ (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM_ (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save c b
 where
  setUser n = getUserEntryForName n >>= setEffectiveUserID . userID

initTVars :: EE -> IO EE
initTVars e = do
  a <- newTQueueIO
  b <- newTVarIO False
  c <- newTVarIO False
  d <- newTVarIO mempty
  return $ e
    & stm.work .~ a
    & stm.running .~ b
    & stm.sudoing .~ c
    & stm.repos .~ d


-- | Run single task with supplied environment. Also signals to scheduler when work is done.
runTask :: EE -> ES -> Free (EL SA s) a -> IO ()
runTask e s t = do
  reify e ((`evalStateT` s) . untag . asProxyOf (task t))
  atomically (writeTQueue (e^.stm.work) Stop)


-- | Thread `s' parameter to 'task' function
asProxyOf :: Execution s () -> Proxy s -> Execution s ()
asProxyOf a _ = a
{-# INLINE asProxyOf #-}


-- | Run single task command by command
--
-- "Forks" on 'Task' 'True'.
-- Note: current thread continues to execute what's inside task, but all the other stuff is queued
--
-- Complexity comes from forking and responding to errors. Otherwise that is dead simple function
task :: Reifies t EE => Free (EL SA s) a -> Execution t ()
task (Free (EP _ _ b d)) = do
  newTask d
  task b
task a@(Free c@(Biegunka.Language.ES _ _ b d)) = do
  e <- try (command c)
  case e of
    Left e' -> do
      r <- retry e'
      case r of
        Retry    -> task a
        Abortive -> return ()
        Ignorant -> do
          newTask d
          task b
    Right _ -> do
      newTask d
      task b
task a@(Free c) = do
  e <- try (command c)
  case e of
    Left e' -> do
      r <- retry e'
      case r of
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
retry :: forall s. Reifies s EE => SomeException -> Execution s React
retry exc = do
  log <- fmap (\e -> e^.controls.logger) reflected
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
reaction :: forall s. Reifies s EE => Execution s React
reaction = head . (++ [_react $ reflect (Proxy :: Proxy s)]) <$> use reactStack


-- | Single command execution
command :: forall a s t. Reifies t EE => EL SA s a -> Execution t ()
command (EW (Reacting (Just r)) _) = reactStack %= (r :)
command (EW (Reacting Nothing) _)  = reactStack %= drop 1
command (EW (User     (Just u)) _) = usersStack %= (u :)
command (EW (User     Nothing) _)  = usersStack %= drop 1
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
      for_ (fmap describe (action c)) log
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
      for_ (fmap describe (action c)) log
      o
      setEffectiveUserID uid
      atomically $ writeTVar stv False
 where
  op :: EL SA s a -> Execution t (IO ())
  op (Biegunka.Language.ES _ (Source _ _ dst update) _ _) = do
    rstv <- liftM (\e -> e^.stm.repos) reflected
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
  op (EA _ (Link src dst) _) = return $ overWriteWith createSymbolicLink src dst
  op (EA _ (Copy src dst) _) = return $ overWriteWith copyFile src dst
  op (EA _ (Template src dst substitute) _) = return $
    let ts = _templates $ reflect (Proxy :: Proxy t) in case ts of
      Templates ts' -> overWriteWith (\s d -> toStrict . substitute ts' . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
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
newTask :: forall a s t. Reifies t EE => Free (EL SA s) a -> Execution t ()
newTask t = do
  e <- reflected
  s <- get
  liftIO . atomically $ writeTQueue (e^.stm.work) (Do $ runTask e s t)

-- | Schedule tasks
--
-- "Forks" on every incoming workload
schedule :: TQueue Work -> IO ()
schedule j = loop (0 :: Int)
 where
  loop n
    | n < 0     = return ()
    | otherwise = atomically (readTQueue j) >>= \t -> case t of
        Do w -> forkIO w >> loop (n + 1)
        Stop ->             loop (n - 1)
