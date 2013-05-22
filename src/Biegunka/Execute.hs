{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import           Control.Concurrent.STM (atomically)
import           Control.Lens hiding (op)
import           Control.Monad.State (evalStateT, runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (def)
import           Data.Proxy
import           Data.Reflection
import           Data.Functor.Trans.Tagged
import           Data.Set (member, insert)
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
import Biegunka.Language (IL(..), A(..), W(..), React(..))
import Biegunka.Transform (simplified)


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
  let s' = simplified s
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
runTask :: EE -> ES -> [IL] -> IO ()
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
task :: forall s. Reifies s EE => [IL] -> Execution s ()
task (IT xs : cs) = newTask cs >> task xs
task t@(c:cs) = do
  e <- try (command c)
  case e of
    Left e' -> respond e' t >>= task
    Right _ -> task cs
task [] = return ()

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
respond :: forall s. Reifies s EE => SomeException -> [IL] -> Execution s [IL]
respond exc t = do
  log <- fmap (\e -> e^.controls.logger) reflected
  liftIO . log . describe $ exception exc
  rc <- retryCount <<%= (+1)
  if rc < _retries (reflect (Proxy :: Proxy s)) then do
    liftIO . log . describe $ retryCounter (rc + 1)
    return t
  else do
    retryCount .= 0
    r <- reaction
    return $ case r of
      Ignorant -> ignoring t
      Abortive -> []

-- | Get current reaction setting from environment
--
-- Note: 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s EE => Execution s React
reaction = head . (++ [_react $ reflect (Proxy :: Proxy s)]) <$> use reactStack

-- | If failure happens to be in emerging 'Source' then we need to skip
-- all related 'Files' operations too.
ignoring :: [IL] -> [IL]
ignoring (IS {} : cs) = go [] cs
 where
  go ws cs'@(IS {} : _)  = reverse ws ++ cs'
  go ws (w@(IW s) : cs') = case s of
    User     (Just _) -> go (w:ws) cs'
    Reacting (Just _) -> go (w:ws) cs'
    _                 -> go [] cs'
  go _  (_    : cs')      = go [] cs'
  go _  []                = []
ignoring (_ : cs)    = cs
ignoring [] = error "Should not been here."


-- | Single command execution
command :: forall s. Reifies s EE => IL -> Execution s ()
command (IW (Reacting (Just r))) = reactStack %= (r :)
command (IW (Reacting Nothing))  = reactStack %= drop 1
command (IW (User     (Just u))) = usersStack %= (u :)
command (IW (User     Nothing))  = usersStack %= drop 1
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
  op (IS dst _ update _ _) = do
    rstv <- liftM (\e -> e^.stm.repos) reflected
    return $ do
      unmentioned <- atomically $ do
        rs <- readTVar rstv
        if dst `member` rs
          then return False
          else do
            writeTVar rstv $ insert dst rs
            return True
      when unmentioned $ do
        createDirectoryIfMissing True $ dropFileName dst
        update
  op (IA (Link src dst) _ _ _ _) = return $ overWriteWith createSymbolicLink src dst
  op (IA (Copy src dst) _ _ _ _) = return $ overWriteWith copyFile src dst
  op (IA (Template src dst substitute) _ _ _ _) = return $
    let ts = _templates $ reflect (Proxy :: Proxy s) in case ts of
      Templates ts' -> overWriteWith (\s d -> toStrict . substitute ts' . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  op (IA (Shell p sc) _ _ _ _) = return $ do
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
newTask :: forall s. Reifies s EE => [IL] -> Execution s ()
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
