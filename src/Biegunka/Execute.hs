{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Execute (execute) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.Foldable (traverse_)
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
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Sequence (Seq)
import qualified Data.Sequence as Q
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
import Biegunka.Language (EL(..), A(..), S(..), M(..), React(..), peek)
import Biegunka.Script


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
  let b = construct s
  a <- load c s
  (controls .~ c -> e') <- initTVars e
  when (e' ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  runTask e' def newTask s
  atomically (writeTQueue (e'^.stm.work) (Stop 0)) -- Can we assume script starts from id 0?
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
task (Free c@(ES _ _ b d)) = do
  newTask d
  e <- try (command c)
  case e of
    Left e' -> do
      r <- retry e'
      case r of
        Retry    -> task (Free (Pure () <$ c))
        Abortive -> return ()
        Ignorant -> task b
    Right _ -> task b
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
reaction :: forall s. Reifies s EE => Execution s React
reaction = head . (++ [_react $ reflect (Proxy :: Proxy s)]) <$> use reactStack


-- | Single command execution
command :: forall a s t. Reifies t EE => EL SA s a -> Execution t ()
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
newTask :: forall a s t. Reifies t EE => Free (EL SA s) a -> Execution t ()
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

-- | Prepares environment to run task with given execution routine
runTask :: forall s a.
          EE -- ^ Environment
        -> EC -- ^ Context
        -> (forall t. Reifies t EE => Free (EL SA s) a -> Execution t ()) -- ^ Task routine
        -> (Free (EL SA s) a) -- ^ Task contents
        -> IO ()
runTask e s f i =
  reify e ((`evalStateT` s) . untag . asProxyOf (f i))

-- | Schedule tasks
--
-- "Forks" on every incoming workload
schedule :: TQueue Work -> IO ()
schedule j = loop 0 IM.empty IM.empty
 where
  loop :: Int -> IntMap Int -> IntMap (Seq (IO ())) -> IO ()
  loop n a b
    | n < 0     = return ()
    | otherwise = atomically (readTQueue j) >>= \t -> case t of
        Do i w -> do
          let n' = n + 1
              a' = a & at i . non 0 +~ 1
          case a ^. at i of
            Nothing -> forkIO w >> loop n' a' b
            Just _  -> loop n' a' (b & at i . anon Q.empty Q.null %~ (|> w))
        Stop i -> do
          let n' = n - 1
              a' = a & at i . non 0 -~ 1
          case b ^? ix i . to uncons . traverse of
            Just (w, _) -> forkIO w >> loop n' a' (b & at i . anon Q.empty Q.null %~ Q.drop 1)
            Nothing -> loop n' a' b
