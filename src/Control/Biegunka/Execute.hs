{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
-- | Real run interpreter
module Control.Biegunka.Execute
  ( run
  , runDiff
  ) where

import           Control.Applicative
import           Control.Concurrent.STM (atomically, retry)
import           Control.Concurrent.STM.TVar (readTVar, modifyTVar, writeTVar)
import           Control.Exception (SomeAsyncException(SomeAsyncException))
import           Control.Lens hiding (op)
import           Control.Lens.Extras (is)
import           Control.Monad
import           Control.Monad.Catch
  ( SomeException
  , fromException, bracket, bracket_, onException, throwM, tryJust
  )
import           Control.Monad.Free (Free(..))
import           Control.Monad.Reader (ask)
import           Control.Monad.Trans (liftIO)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Char8 as ByteString
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import           Prelude hiding (log, null)
import qualified System.Directory as D
import           System.Exit.Lens (_ExitFailure)
import           System.FilePath (dropFileName, takeFileName, addExtension)
import           System.Environment (getEnvironment)
import qualified System.IO as IO
import qualified System.IO.Error as IO
import qualified System.Posix as Posix
import qualified System.Process as P
import           Text.Printf (printf)

import qualified Control.Biegunka.Logger as Logger
import           Control.Biegunka.Settings
  (Templates(..), templates, Mode(..), mode)
import qualified Control.Biegunka.Namespace as Ns
import           Control.Biegunka.Execute.Describe
  (prettyTerm, sourceIdentifier, prettyDiff, removal)
import           Control.Biegunka.Execute.Exception
import qualified Control.Biegunka.Execute.IO as EIO
import           Control.Biegunka.Execute.Settings
import           Control.Biegunka.Language
import           Control.Biegunka.Interpreter (Interpreter, optimistically)
import qualified Control.Biegunka.Execute.Watcher as Watcher
import qualified Control.Biegunka.Patience as Patience
import           Control.Biegunka.Script
import           Control.Biegunka.Templates (templating)

{-# ANN module "HLint: ignore Use const" #-}


-- | Real run interpreter
run :: Interpreter
run = optimistically go where
  go settings s = do
    let db' = Ns.fromScript s
    Ns.withDb settings $ \db -> do
      mapM_ (safely remove)          (Ns.diff (map Ns.filePath . Ns.files)     (view Ns.namespaces db) db')
      mapM_ (safely removeDirectory) (Ns.diff (map Ns.sourcePath . Ns.sources) (view Ns.namespaces db) db')
      bracket Posix.getEffectiveUserID Posix.setEffectiveUserID $ \_ ->
        bracket Posix.getEffectiveGroupID Posix.setEffectiveGroupID $ \_ ->
          withExecution settings $ \e ->
            runExecutor e (forkExecutor (execute s))
      Ns.commit db db'
   where
    remove path =
      D.doesFileExist path >>= \case
        True  -> do
          Logger.write IO.stdout settings (removal path)
          D.removeFile path
        False -> D.removeDirectoryRecursive path

    removeDirectory path =
      D.doesDirectoryExist path >>= \case
        True  -> do
          Logger.write IO.stdout settings (removal path)
          D.removeDirectoryRecursive path
        False -> return ()

    safely doThings = IO.tryIOError . doThings

-- | Show the diff between the current state of the system and the
-- result of running the script.
runDiff :: Interpreter
runDiff = optimistically go where
  go settings s =
    bracket Posix.getEffectiveUserID Posix.setEffectiveUserID $ \_ ->
      bracket Posix.getEffectiveGroupID Posix.setEffectiveGroupID $ \_ ->
        withExecution settings $ \e ->
          runExecutor (set onlyDiff True e) (forkExecutor (execute s))

-- | Execute a single task.
--
-- Sources are executed concurrently, while Actions are executed sequentially.
-- Every action may retry the specified amount of times, and retries do not accumulate
-- across actions.
execute :: Term Annotate s () -> Executor ()
execute (Free c@(TS (AS { asToken, asMaxRetries, asReaction }) _ b t)) = do
  forkExecutor (execute t)
  flip fix (Retries 0) $ \loop rs ->
    executeIO (doGenIO rs) c >>= \case
      False ->
        if rs < asMaxRetries
          then loop (incr rs)
          else case asReaction of
            Abortive -> doneWith asToken
            Ignorant -> do
              execute b
              doneWith asToken
      True -> do
        execute b
        doneWith asToken
execute (Free c@(TA (AA { aaMaxRetries, aaReaction }) _ x)) =
  flip fix (Retries 0) $ \loop rs ->
    executeIO (doGenIO rs) c >>= \case
      False ->
        if rs < aaMaxRetries
          then loop (incr rs)
          else case aaReaction of
            Abortive -> return ()
            Ignorant -> execute x
      True -> execute x
execute (Free c@(TWait _ x)) = do
  executeIO (doGenIO (Retries 0)) c
  execute x
execute (Pure _) = return ()

-- | Execute a single command.
executeIO
  :: (TermF Annotate s a -> Executor (IO Bool)) -> TermF Annotate s a -> Executor Bool
executeIO _ (TWait waits _) = do
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

doGenIO :: Retries -> TermF Annotate s a -> Executor (IO Bool)
doGenIO retries t = do
  e  <- ask
  genIO t <&> \io ->
    trySynchronous io >>= \case
      Left exc -> do
        logMessage e IO.stderr (Left exc)
        return False
      Right (diff, finish)
        | view onlyDiff e -> do
          for_ diff (logMessage e IO.stdout . pure . pure)
          return True
        | otherwise -> do
          res <- trySynchronous finish
          logMessage e
                     (either (\_ -> IO.stderr) (\_ -> IO.stdout) res)
                     (fmap (\finalDiff -> finalDiff <|> diff) res)
          return (either (\_ -> False) (\_ -> True) res)
 where
  logMessage e stream diff =
    for_ (sourceIdentifier t) $ \i ->
      atomically $ do
        ms <- readTVar (view activeSource e)
        Logger.writeSTM stream
                        e
                        (prettyTerm retries diff (maybe True (/= i) ms) t)
        writeTVar (view activeSource e) (Just i)
{-# ANN doGenIO "HLint: ignore Avoid lambda" #-}
{-# ANN doGenIO "HLint: ignore Use >=>" #-}

-- | Catch all synchronous exceptions.
trySynchronous :: IO a -> IO (Either SomeException a)
trySynchronous =
  tryJust (\e ->
    case fromException e of
      Just (SomeAsyncException _) -> Nothing
      _ -> Just e)

genIO :: TermF Annotate s a -> Executor (IO ([DiffItem], IO [DiffItem]))
genIO term = case term of
  TS _ (Source _ _ dst update) _ _ ->
    view mode >>= \case
      Offline -> return (return (empty, return empty))
      Online  -> do
        rstv <- view repos
        return $ do
          updated <- atomically $ do
            rs <- readTVar rstv
            if dst `Set.member` rs
              then return True
              else do
                writeTVar rstv (Set.insert dst rs)
                return False
          if updated
            then return (empty, return empty)
            else do
              D.createDirectoryIfMissing True (dropFileName dst)
              update dst
             `onException`
              atomically (modifyTVar rstv (Set.delete dst))

  TA _ (Link src dst) _ -> return $ do
    msg <- IO.tryIOError (Posix.readSymbolicLink dst) <&> \case
      Left _ -> pure (diffItemHeaderOnly (printf "linked to ‘%s’" src))
      Right src'
        | src /= src' -> pure (diffItemHeaderOnly (printf "relinked from ‘%s’ to ‘%s’" src' src))
        | otherwise   -> empty
    return
      ( msg
      , empty <$ do EIO.prepareDestination dst; Posix.createSymbolicLink src dst
      )

  TA _ (Copy src dst) _ -> return $ do
    msg <- fmap (fmap showDiff)
                (EIO.compareContents (Proxy :: Proxy Hash.SHA1) src dst)
    return
      ( maybe empty pure msg
      , empty <$ do EIO.prepareDestination dst; D.copyFile src dst
      )

  TA _ (Template src dst) _ -> do
    Templates ts <- view templates
    td <- view tempDir
    return $ do
      (tempfp, h) <- IO.openTempFile td (addExtension (takeFileName src) "tmp")
      IO.hClose h
      Text.writeFile tempfp . templating ts =<< Text.readFile src
      msg <- fmap (fmap showDiff)
                  (EIO.compareContents (Proxy :: Proxy Hash.SHA1) tempfp dst)
      return
        ( maybe empty pure msg
        , empty <$ do
            EIO.prepareDestination dst
            D.renameFile tempfp dst `IO.catchIOError` \_ -> D.copyFile tempfp dst
        )

  TA ann (Command p spec) _ -> return (return (empty, empty <$ cmd))
   where
    cmd = do
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
                       , "SOURCE_ROOT"
                       ])
          , P.std_in        = P.Inherit
          , P.std_out       = P.CreatePipe
          , P.std_err       = P.CreatePipe
          , P.close_fds     = False
          , P.create_group  = False
          , P.delegate_ctlc = False
          }
      e <- P.waitForProcess ph
      IO.hClose out
      forOf_ _ExitFailure e (\status -> throwM . ShellException status =<< Text.hGetContents err)
    xs `except` ys = filter (\x -> fst x `notElem` ys) xs

  TWait _ _ -> return (return (empty, return empty))
 where
  showDiff :: Either (Hash.Digest a) (Hash.Digest a, Hash.Digest a, Patience.FileDiff) -> DiffItem
  showDiff =
    either (diffItemHeaderOnly . printf "contents changed from ‘none’ to ‘%s’" . showHash)
           (\(x, y, d) -> DiffItem
             { diffItemHeader = printf "contents changed from ‘%s' to ‘%s’" (showHash x) (showHash y)
             , diffItemBody   = prettyDiff d
             })
  showHash = take 8 . ByteString.unpack . Hash.digestToHexByteString

-- | Tell execution process that you're done with task
doneWith :: Token -> Executor ()
doneWith tok = do
  watcher <- view watch
  Watcher.done watcher tok

-- | Get user associated with term
getUser :: TermF Annotate s a -> Maybe User
getUser (TS (AS { asUser }) _ _ _) = asUser
getUser (TA (AA { aaUser }) _ _) = aaUser
getUser (TWait _ _) = Nothing

userID :: User -> IO Posix.UserID
userID (UserID i)   = return i
userID (Username n) = Posix.userID <$> Posix.getUserEntryForName n

userGroupID :: User -> IO Posix.GroupID
userGroupID (UserID i)   = Posix.userGroupID <$> Posix.getUserEntryForID i
userGroupID (Username n) = Posix.userGroupID <$> Posix.getUserEntryForName n
