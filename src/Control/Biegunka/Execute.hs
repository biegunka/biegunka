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
import           Control.Concurrent (forkFinally)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Concurrent.STM (atomically, retry)
import           Control.Exception (SomeAsyncException(SomeAsyncException))
import           Control.Lens hiding (op)
import           Control.Lens.Extras (is)
import           Control.Monad
import           Control.Monad.Catch
  ( SomeException
  , fromException, bracket, bracket_, onException, throwM, tryJust
  )
import           Control.Monad.Free (Free(..))
import           Control.Monad.Reader (runReaderT, ask)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Default.Class (Default(..))
import qualified Data.Set as S
import qualified Data.Text.IO as T
import           Prelude hiding (log, null)
import qualified System.Directory as D
import           System.FilePath (dropFileName)
import           System.Environment (getEnvironment)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as Posix
import qualified System.Process as P
import           Text.Printf (printf)

import           Control.Biegunka.Action (copy)
import qualified Control.Biegunka.Log as Log
import           Control.Biegunka.Settings
  (Settings, Templates(..), templates, local, logger, Mode(..), mode)
import qualified Control.Biegunka.Namespace as Ns
import           Control.Biegunka.Execute.Settings
import           Control.Biegunka.Execute.Describe
  (runChanges, describeTerm, removal)
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
    let db' = Ns.fromScript s
    bracket (Ns.open settings) Ns.close $ \db -> do
      mapM_ (safely remove)          (Ns.diff Ns.files   (view Ns.these db) db')
      mapM_ (safely removeDirectory) (Ns.diff Ns.sources (view Ns.these db) db')
      bracket Posix.getEffectiveUserID Posix.setEffectiveUserID $ \_ ->
        bracket Posix.getEffectiveGroupID Posix.setEffectiveGroupID $ \_ ->
          bracket Watcher.new Watcher.wait $ \watcher -> do
            r <- initializeSTM watcher
            runTask (set local r settings) (task (views mode io settings) def) s
      Ns.commit (db & Ns.these .~ db')
   where
    io Offline = runAction
    io Online  = runAll

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

    safely doThings = IO.tryIOError . doThings

-- | Dry run interpreter
dryRun :: Interpreter
dryRun = interpretOptimistically $ \settings s -> do
  let db' = Ns.fromScript s
  bracket (Ns.open settings) Ns.close $ \db -> do
    bracket Watcher.new Watcher.wait $ \watcher -> do
      r <- initializeSTM watcher
      runTask (set local r settings) (task runPure def) s
    Log.write (view logger settings) $
      Log.plain (runChanges db db')


-- | Prepares environment to run task with given execution routine
runTask
  :: forall s m. MonadIO m
  => Settings Execution
  -> (Free (Term Annotate s) () -> Executor ())
  -> Free (Term Annotate s) () -- ^ Task contents
  -> m ()
runTask e f s = do
  Watcher.register (view watch e)
  liftIO (forkFinally (runReaderT (f s) e)
                      (\_ -> Watcher.unregister (view watch e)))
  return ()


-- | Run a single task
--
-- "Forks" on 'TS'.
-- Note: current thread continues to execute what's inside the
-- task, but all the other stuff is queued for execution in scheduler
task
  :: (forall a t. Retries -> Term Annotate t a -> Executor (IO Bool))
  -> Retries
  -> Free (Term Annotate s) ()
  -> Executor ()
task f = go
 where
  go retries (Free c@(TS _ _ _ t@(Free _))) = do
    e <- ask
    runTask e (task f def) t
    go retries (Free (Pure () <$ c))
  go retries (Free c@(TS (AS { asToken, asMaxRetries }) _ b (Pure _))) =
    executeIO (f retries) c >>= \case
      False -> do
        if retries < asMaxRetries
          then go (incr retries) (Free (Pure () <$ c))
          else case getReaction c of
            Abortive -> doneWith asToken
            Ignorant -> do
              task f def b
              doneWith asToken
      True -> do
        task f def b
        doneWith asToken
  go retries a@(Free c@(TA (AA { aaMaxRetries }) _ x)) =
    executeIO (f retries) c >>= \case
      False ->
        if retries < aaMaxRetries
          then go (incr retries) a
          else case getReaction c of
            Abortive -> return ()
            Ignorant -> go def x
      True -> go def x
  go retries (Free c@(TM _ x)) = do
    executeIO (f retries) c
    go def x
  go _ (Pure _) = return ()

-- | Execute a single command.
executeIO
  :: (Term Annotate s a -> Executor (IO Bool)) -> Term Annotate s a -> Executor Bool
executeIO _ (TM (Wait waits) _) = do
  watcher <- view watch
  Watcher.waitDone watcher waits
  return True
executeIO getIO term = do
  users <- view user
  io    <- getIO term
  liftIO $ case getUser term of
    Nothing ->
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

runAll :: Retries -> Term Annotate s a -> Executor (IO Bool)
runAll retries term = do
  log <- view logger
  io  <- ioOnline term
  return $ do
    res <- trySynchronous io
    logTerm log retries res term
    return (either (\_ -> False) (\_ -> True) res)

-- | Log an action and its outcome.
logTerm
  :: Log.Logger
  -> Retries
  -> Either SomeException (Maybe String)
  -> Term Annotate s a
  -> IO ()
logTerm log retries out =
  Log.write log . stream . describeTerm retries out
 where
  stream = either (\_ -> Log.exception) (\_ -> Log.plain) out

-- | Catch all synchronous exceptions.
trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous =
  tryJust (\e ->
    case fromException e of
      Just (SomeAsyncException _) -> Nothing
      _ -> Just e)

ioOnline :: Term Annotate s a -> Executor (IO (Maybe String))
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
      if updated
        then return Nothing
        else do
          D.createDirectoryIfMissing True $ dropFileName dst
          update dst
     `onException`
      atomically (modifyTVar rstv (S.delete dst))

  TA _ (Link src dst) _ -> return $ do
    msg <- IO.tryIOError (Posix.readSymbolicLink dst) <&> \case
      Left _ -> Just (printf "linked to ‘%s’" src)
      Right src'
        | src /= src' -> Just (printf "relinked from ‘%s’ to ‘%s’" src' src)
        | otherwise   -> Nothing
    overWriteWith Posix.createSymbolicLink src dst
    return msg

  TA _ (Copy src dst spec) _ -> return $ do
    IO.tryIOError (D.removeDirectoryRecursive dst)
    D.createDirectoryIfMissing True $ dropFileName dst
    copy src dst spec
    return Nothing

  TA _ (Template src dst substitute) _ -> do
    Templates ts <- view templates
    return $ do
      overWriteWith (\s d -> T.writeFile d . substitute ts =<< T.readFile s) src dst
      return Nothing

  TA ann (Command p spec) _ -> return $ do
    defenv <- getEnvironment
    (_, Just out, Just err, ph) <- P.createProcess
      P.CreateProcess
        { P.cmdspec       = spec
        , P.cwd           = Just p
        , P.env =
            Just ( ("RUN_ROOT",    view runRoot    ann)
                 : ("SOURCE_ROOT", view sourceRoot ann)
                 : defenv `except`
                     [ "CABAL_SANDBOX_CONFIG"
                     , "CABAL_SANDBOX_PACKAGE_PATH"
                     , "GHC_PACKAGE_PATH"
                     , "RUN_ROOT"
                     , "SOURCE_ROOT"])
        , P.std_in        = P.Inherit
        , P.std_out       = P.CreatePipe
        , P.std_err       = P.CreatePipe
        , P.close_fds     = False
        , P.create_group  = False
        , P.delegate_ctlc = False
        }
    e <- P.waitForProcess ph
    IO.hClose out
    e `onFailure` \status ->
      T.hGetContents err >>= throwM . ShellException spec status
    return Nothing
   where
    xs `except` ys = filter (\x -> fst x `notElem` ys) xs

  TM _ _ -> return $ return Nothing
 where
  overWriteWith g src dst = do
    D.createDirectoryIfMissing True (dropFileName dst)
    IO.tryIOError (Posix.removeLink dst) -- removeLink throws an exception if the file is missing
    g src dst

runAction :: Retries -> Term Annotate s a -> Executor (IO Bool)
runAction r t@(TS {}) = runPure r t
runAction r t         = runAll r t

runPure :: Applicative m => a -> Term Annotate s b -> Executor (m Bool)
runPure _ _ = pure (pure True)

-- | Tell execution process that you're done with task
doneWith :: Token -> Executor ()
doneWith tok = do
  watcher <- view watch
  Watcher.done watcher tok


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

userID :: User -> IO Posix.UserID
userID (UserID i)   = return i
userID (Username n) = Posix.userID <$> Posix.getUserEntryForName n

userGroupID :: User -> IO Posix.GroupID
userGroupID (UserID i)   = Posix.userGroupID <$> Posix.getUserEntryForID i
userGroupID (Username n) = Posix.userGroupID <$> Posix.getUserEntryForName n
