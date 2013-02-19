{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.Execute (execute, BiegunkaException(..)) where

import           Control.Applicative
import           Control.Monad (when)
import           Control.Exception (Exception, SomeException(..), throwIO)
import qualified Control.Exception as E
import           Data.List ((\\))
import           Data.Monoid ((<>))
import           Data.Foldable (traverse_)
import           Data.Typeable (Typeable)
import           System.Exit (ExitCode(..))
import           System.IO.Error (catchIOError, tryIOError)

import           Control.Lens hiding (Action)
import           Data.Default (def)
import           Control.Monad.State (evalStateT, runStateT, get, put)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Proxy
import           Data.Reflection
import           Data.Text (Text)
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
import           System.Posix.User (getEffectiveUserName, getUserEntryForName, userID, setEffectiveUserID)
import           System.Process (system)

import Biegunka.Control (Interpreter(..), Task, root)
import Biegunka.DB
import Biegunka.Execute.Narrator
import Biegunka.Execute.Control
import Biegunka.Language (Command(..), Action(..), Wrapper(..), React(..))


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- Supports some options
--
-- @
-- main :: IO ()
-- main = execute (def & react .~ Ignorant) $ do
--   profile ...
--   profile ...
-- @
execute :: EE -> Interpreter
execute e = I $ \c (concat -> s) -> do
  let b = construct s
  a <- load (c ^. root) s
  when (e ^. priviledges == Drop) $ getEnv "SUDO_USER" >>= traverse_ setUser
  n <- narrator (_volubility e)
  runTask e { _narrative = Just n } s
  mapM (tryIOError . removeFile) (filepaths a \\ filepaths b)
  mapM (tryIOError . removeDirectoryRecursive) (sources a \\ sources b)
  save (c ^. root) b


-- | Run single task with supplied environment
runTask :: EE -> Task l b -> IO ()
runTask e t = reify e ((`evalStateT` def) . runE . asProxyOf (task t))


-- | Thread `s' parameter to 'task' function
asProxyOf :: Execution s () -> Proxy s -> Execution s ()
asProxyOf a _ = a


-- | Custom execptions
data BiegunkaException =
    ShellCommandFailure String
  | SourceEmergingFailure String FilePath Text
    deriving (Typeable)


instance Show BiegunkaException where
  show = T.unpack . T.unlines . filter (not . T.null) . T.lines . pretty
   where
    pretty (ShellCommandFailure t) =
      "Biegunka has failed to execute `" <> T.pack t <> "`"
    pretty (SourceEmergingFailure up fp fs) =
      "Biegunka has failed to emerge source " <> T.pack up <> " in " <> T.pack fp <> "\nFailures log:\n" <> fs

instance Exception BiegunkaException


-- | Run single task command by command
-- Complexity comes from responding to errors. User may control reaction via
-- 'react' lens to 'EE'. Here he would be asked for prompt if needed or just
-- notified about errors if not
task :: forall l b s. Reifies s EE => Task l b -> Execution s ()
task t@(c:cs) = do
  e <- try (execute' c)
  case e of
    Left e' -> respond e' t >>= task
    Right _ -> task cs
task [] = return ()

-- | If only I could come up with MonadBaseControl instance for Execution
try :: Exception e => Execution s a -> Execution s (Either e a)
try (E ex) = do
  eeas <- io . E.try . runStateT ex =<< get
  case eeas of
    Left e       ->          return (Left e)
    Right (a, s) -> put s >> return (Right a)

-- | Get response from task failure processing
respond :: forall l b s. Reifies s EE
        => SomeException -> Task l b -> Execution s (Task l b)
respond e t = do
  io . putStrLn $ "FAIL: " ++ show e
  rc <- use retryCount
  if rc < view retries (reflect (Proxy :: Proxy s)) then do
    io . putStrLn $ "Retry: " ++ show (rc + 1)
    retryCount += 1
    return t
  else do
    retryCount .= 0
    r <- reaction
    return $ case r of
      Ignorant -> ignore t
      Abortive -> []

-- | Get current reaction setting from environment
-- 'head' is safe here because list is always non-empty
reaction :: forall s. Reifies s EE => Execution s React
reaction = head . (++ [view react $ reflect (Proxy :: Proxy s)]) <$> use reactStack

-- | If failure happens to be in emerging 'Source' then we need to skip
-- all related 'Files' operations too.
ignore :: Task l b -> Task l b
ignore (S {} : cs) = dropCommands skip cs
 where
  skip (P {} : _) = False
  skip (S {} : _) = False
  skip (W {} : cs') = skip cs'
  skip _ = True
ignore (_ : cs)    = cs
ignore [] = error "Should not been here."


-- | Single command execution
execute' :: forall l b s. Reifies s EE => Command l () b -> Execution s ()
execute' c = case c of
  S _ url path _ update _ -> do
    narrate (Typical $ "Emerging source: " ++ url)
    io $ update path

  F (Link src dst) _       -> io $ overWriteWith createSymbolicLink src dst
  F (Copy src dst) _       -> io $ overWriteWith copyFile src dst
  F (Template src dst substitute) _ -> do
    Templates ts <- return $ view templates (reflect (Proxy :: Proxy s))
    io $ overWriteWith (\s d -> toStrict . substitute ts . T.unpack <$> T.readFile s >>= T.writeFile d) src dst
  F (Shell p sc) _         -> io $ do
    d <- getCurrentDirectory
    setCurrentDirectory p
    flip catchIOError (\_ -> throwIO $ ShellCommandFailure sc) $ do
      e <- system sc
      case e of
        ExitFailure _ -> throwIO $ ShellCommandFailure sc
        _ -> return ()
    setCurrentDirectory d

  W (Reacting (Just r)) _  -> reactStack %= (r :)
  W (Reacting Nothing) _   -> reactStack %= drop 1
  W (User (Just n)) _      -> io getEffectiveUserName >>= \u -> setUser n >> userStack %= (u :)
  W (User Nothing) _       -> use userStack >>= \(u:us) -> setUser u >> userStack .= us

  _ -> return ()
 where
  overWriteWith g src dst = do
    createDirectoryIfMissing True $ dropFileName dst
    tryIOError (removeLink dst) -- needed because removeLink throws an unintended exception if file is absent
    g src dst


dropCommands :: ([Command l a b] -> Bool) -> [Command l a b] -> [Command l a b]
dropCommands f p@(_:cs)
  | f p = dropCommands f cs
  | otherwise    = p
dropCommands _ [] = []


setUser :: MonadIO m => String -> m ()
setUser n = io $ getUserEntryForName n >>= setEffectiveUserID . userID


io :: MonadIO m => IO a -> m a
io = liftIO
