{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Execute (execute) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.List ((\\), delete)
import           Data.Foldable (traverse_)
import           System.Exit (ExitCode(..))
import           System.IO.Error (catchIOError, tryIOError)

import           Control.Concurrent.Async
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Lens hiding (Action)
import           Control.Monad.State (evalStateT, runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default (def)
import           Data.Proxy
import           Data.Reflection
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

import Biegunka.Control (Interpreter(..))
import Biegunka.DB
import Biegunka.Execute.Control
import Biegunka.Execute.Exception
import Biegunka.Execute.Narrator
import Biegunka.Language.Internal
import Biegunka.Language.External (Action(..), Wrapper(..), React(..))


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
  when (e ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  n <- narrator (_volubility e)
  w <- newChan
  writeChan w (Do $ runTask e { _narrative = n, _work = w } def s >> writeChan w Stop)
  scheduler w (e ^. order)
  mapM_ (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM_ (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save c b
 where
  setUser n = getUserEntryForName n >>= setEffectiveUserID . userID


-- | Run single task with supplied environment. Also signals to scheduler when work is done.
runTask :: EE -> ES -> [IL] -> IO ()
runTask e s t = reify e ((`evalStateT` s) . runE . asProxyOf (task t)) >> writeChan (e ^. work) Stop


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
task (IW (Task True) : cs) = do
  let (a, b) = yielding cs
  newTask b
  task a
task t@(c:cs) = do
  e <- try (command c)
  case e of
    Left e' -> respond e' t >>= task
    Right _ -> task cs
task [] = return ()

-- | If only I could come up with MonadBaseControl instance for Execution
try :: Exception e => Execution s a -> Execution s (Either e a)
try (E ex) = do
  eeas <- liftIO . E.try . runStateT ex =<< get
  case eeas of
    Left e       ->          return (Left e)
    Right (a, s) -> put s >> return (Right a)

-- | Get response from task failure processing
--
-- Possible responses: retry command execution or ignore failure or abort task
respond :: forall s. Reifies s EE => SomeException -> [IL] -> Execution s [IL]
respond e t = do
  liftIO . putStrLn $ "FAIL: " ++ show e
  rc <- use retryCount
  if rc < _retries (reflect (Proxy :: Proxy s)) then do
    liftIO . putStrLn $ "Retry: " ++ show (rc + 1)
    retryCount += 1
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
    Task     True     -> go (w:ws) cs'
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
  let sudoingTV = _sudoing $ reflect (Proxy :: Proxy s)
      runningTV = _running $ reflect (Proxy :: Proxy s)
  xs <- use usersStack
  o  <- action c
  liftIO $ case xs of
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
  action (IS dst _ update _ _ src) = do
    narrate (Typical $ "Emerging source: " ++ src)
    liftIO $ createDirectoryIfMissing True $ dropFileName dst
    return update
  action (IA (Link src dst) _ _ _) = return $ overWriteWith createSymbolicLink src dst
  action (IA (Copy src dst) _ _ _) = return $ overWriteWith copyFile src dst
  action (IA (Template src dst substitute) _ _ _) = return $
    let ts = _templates $ reflect (Proxy :: Proxy s) in case ts of
      Templates ts' -> overWriteWith (\s d -> toStrict . substitute ts' . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  action (IA (Shell p sc) _ _ _) = return $ do
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


-- | Separate current task into two
yielding :: [IL] -> ([IL], [IL])
yielding = go [] 1
 where
  go :: [IL] -> Int -> [IL] -> ([IL], [IL])
  go acc 1 (   IW (Task False)  : xs) = (reverse acc, xs)
  go acc n (x@(IW (Task False)) : xs) = go (x : acc) (n - 1) xs
  go acc n (x@(IW (Task True) ) : xs) = go (x : acc) (n + 1) xs
  go acc n (x                   : xs) = go (x : acc) n xs
  go _   _ []                         = error "Broken Task structure!"

-- | Queue next task in scheduler
newTask :: forall s. Reifies s EE => [IL] -> Execution s ()
newTask t = do
  let e = reflect (Proxy :: Proxy s)
  s <- get
  liftIO $ writeChan (e ^. work) (Do $ runTask e s t)

-- | Task scheduler
--
-- Works a bit differently depending on 'Order'. 'Sequential' forces scheduler to have only one
-- "working thread" that processes all the tasks. 'Concurrent' order forces scheduler to "fork" on
-- every coming workload
scheduler :: Chan Work -> Order -> IO ()
scheduler j o = case o of
  Sequential -> go [] 0 1
  Concurrent -> go [] 0 maxBound
 where
  go :: [Async ()] -> Int -> Int -> IO ()
  go as n 0 = do
    (a, _) <- waitAny as
    go (delete a as) n 1
  go as n k
    | n < 0 = return ()
    | otherwise = do
        t <- readChan j
        case t of
          Do w -> do
            a <- async w
            go (a : as) (n + 1) (k - 1)
          Stop ->
            go      as  (n - 1)  k
