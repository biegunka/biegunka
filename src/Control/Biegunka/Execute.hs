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
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.Maybe (catMaybes)
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
  (Templates(..), templates, Mode(..))
import qualified Control.Biegunka.Namespace as Ns
import           Control.Biegunka.Execute.Describe
  (prettyTerm, sourceIdentifier, removal)
import           Control.Biegunka.Execute.Exception
import qualified Control.Biegunka.Execute.IO as EIO
import           Control.Biegunka.Execute.Settings
import           Control.Biegunka.Language
import           Control.Biegunka.Interpreter (Interpreter, optimistically)
import qualified Control.Biegunka.Execute.Watcher as Watcher
import           Control.Biegunka.Script
import           Control.Biegunka.Templates (templating)

{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}


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
    remove fp =
      D.doesFileExist fp >>= \case
        True  -> do
          Logger.write IO.stdout settings (removal fp)
          D.removeFile fp
        False -> D.removeDirectoryRecursive fp

    removeDirectory fp =
      D.doesDirectoryExist fp >>= \case
        True  -> do
          Logger.write IO.stdout settings (removal fp)
          D.removeDirectoryRecursive fp
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
execute (Free c@(TF (AA { aaMaxRetries, aaReaction }) _ x)) =
  flip fix (Retries 0) $ \loop rs ->
    executeIO (doGenIO rs) c >>= \case
      False ->
        if rs < aaMaxRetries
          then loop (incr rs)
          else case aaReaction of
            Abortive -> return ()
            Ignorant -> execute x
      True -> execute x
execute (Free c@(TC (AA { aaMaxRetries, aaReaction }) _ x)) =
  flip fix (Retries 0) $ \loop rs ->
    executeIO (doGenIO rs) c >>= \case
      False ->
        if rs < aaMaxRetries
          then loop (incr rs)
          else case aaReaction of
            Abortive -> return ()
            Ignorant -> execute x
      True -> execute x
execute (Free c@(TW _ x)) = do
  executeIO (doGenIO (Retries 0)) c
  execute x
execute (Pure _) = return ()

-- | Execute a single command.
executeIO
  :: (TermF Annotate s a -> Executor (IO Bool)) -> TermF Annotate s a -> Executor Bool
executeIO _ (TW waits _) = do
  watcher <- view watch
  Watcher.waitDone watcher waits
  return True
executeIO getIO term = do
  users <- view user
  io    <- getIO term
  liftIO $ if isSudo term then
    bracket_ (acquire users 0) (release users 0) $ do
      Posix.setEffectiveGroupID 0
      Posix.setEffectiveUserID 0
      io
  else io
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
        | view onlyDiff e ->
          True <$ case diff of
            [] -> return ()
            _  -> logMessage e IO.stdout (Right diff)
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

  TF _ tf _ -> case tf of
    FC {} -> copy (view origin tf) (view path tf) (view mode tf) (view owner tf) (view group tf)
    FT {} -> template (view origin tf) (view path tf) (view mode tf) (view owner tf) (view group tf)
    FL {} -> link (view origin tf) (view path tf) (view owner tf) (view group tf)
   where
    copy src dst mode_ owner_ group_ = return $ do
      contentsDiff <- fmap EIO.showContentsDiff (EIO.diffContents src dst)
      modeDiff <- maybe (return Nothing) (fmap EIO.showFileModeDiff . EIO.diffFileMode dst) mode_
      ownerDiff <- maybe (return Nothing) (fmap EIO.showOwnerDiff . EIO.diffOwner dst) owner_
      groupDiff <- maybe (return Nothing) (fmap EIO.showGroupDiff . EIO.diffGroup dst) group_
      return
        ( catMaybes [contentsDiff, modeDiff, ownerDiff, groupDiff]
        , empty <$ do
            EIO.prepareDestination dst
            D.copyFile src dst
            for_ (modeDiff *> mode_) (\m -> Posix.setFileMode dst (m `mod` 0o1000))
            for_ (ownerDiff *> owner_) (EIO.setOwner dst)
            for_ (groupDiff *> group_) (EIO.setGroup dst)
        )

    template src dst mode_ owner_ group_ = do
      Templates ts <- view templates
      td <- view tempDir
      return $ do
        (tempfp, h) <- IO.openTempFile td (addExtension (takeFileName src) "tmp")
        IO.hClose h
        Text.writeFile tempfp . templating ts =<< Text.readFile src
        contentsDiff <- fmap EIO.showContentsDiff (EIO.diffContents tempfp dst)
        modeDiff <- maybe (return Nothing) (fmap EIO.showFileModeDiff . EIO.diffFileMode dst) mode_
        ownerDiff <- maybe (return Nothing) (fmap EIO.showOwnerDiff . EIO.diffOwner dst) owner_
        groupDiff <- maybe (return Nothing) (fmap EIO.showGroupDiff . EIO.diffGroup dst) group_
        return
          ( catMaybes [contentsDiff, modeDiff, ownerDiff, groupDiff]
          , empty <$ do
              EIO.prepareDestination dst
              D.renameFile tempfp dst `IO.catchIOError` \_ -> D.copyFile tempfp dst
              for_ (modeDiff *> mode_) (\m -> Posix.setFileMode dst (m `mod` 0o1000))
              for_ (ownerDiff *> owner_) (EIO.setOwner dst)
              for_ (groupDiff *> group_) (EIO.setGroup dst)
          )

    link src dst owner_ group_ = return $ do
      sourceDiff <- IO.tryIOError (Posix.readSymbolicLink dst) <&> \case
        Left _ -> pure (diffItemHeaderOnly (printf "linked to ‘%s’" src))
        Right src'
          | src /= src' -> pure (diffItemHeaderOnly (printf "relinked from ‘%s’ to ‘%s’" src' src))
          | otherwise   -> empty
      ownerDiff <- maybe (return Nothing) (fmap EIO.showOwnerDiff . EIO.diffOwner dst) owner_
      groupDiff <- maybe (return Nothing) (fmap EIO.showGroupDiff . EIO.diffGroup dst) group_
      return
        ( catMaybes [sourceDiff, ownerDiff, groupDiff]
        , empty <$ do
            EIO.prepareDestination dst
            Posix.createSymbolicLink src dst
            for_ (ownerDiff *> owner_) (EIO.setOwner dst)
            for_ (groupDiff *> group_) (EIO.setGroup dst)
        )

  TC ann (Command p spec) _ -> return (return (empty, empty <$ cmd))
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

  TW _ _ -> return (return (empty, return empty))

-- | Tell execution process that you're done with task
doneWith :: Token -> Executor ()
doneWith tok = do
  watcher <- view watch
  Watcher.done watcher tok

-- | Is ‘sudo’ active?
isSudo :: TermF Annotate s a -> Bool
isSudo (TS (AS { asSudoActive }) _ _ _) = asSudoActive
isSudo (TF (AA { aaSudoActive }) _ _) = aaSudoActive
isSudo (TC (AA { aaSudoActive }) _ _) = aaSudoActive
isSudo (TW _ _) = False
