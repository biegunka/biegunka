{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
-- | Run (or check) biegunka script
module Run
  ( find
  , run
  ) where

import           Control.Concurrent (ThreadId, forkIO, forkFinally, threadDelay, killThread)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Control.Lens hiding ((<.>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Control.Monad (when, unless)
import           Data.Conduit ((=$=), Producer, runConduit, awaitForever, yield)
import qualified Data.Conduit.Filesystem as CF
import qualified Data.Conduit.List as CL
import           Data.Foldable (for_)
import           Data.Function (fix)
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Exit (exitWith)
import           System.Exit.Lens (_ExitFailure)
import           System.FilePath.Lens (directory, filename)
import qualified System.IO as IO
import qualified System.Posix as Posix
import           System.Process
import           Text.Printf (printf)

import           Control.Biegunka.Logger (Logger)
import qualified Control.Biegunka.Logger as Logger

import           Options (scriptName)


-- | Run a script.
--
-- Does a couple of clever things:
--
--   * Uses @cabal exec runhaskell@ to run the script in the sandbox environment.
--
--   * Adds script's parent directory to the paths that ghc searches for imports.
run :: FilePath -> [String] -> IO a
run script args =
  Logger.with $ \logger -> do
    Logger.write IO.stdout logger (printf "Running ‘%s’  " script)
    let (scriptArgs, ghcArgs) = List.partition ("--" `List.isPrefixOf`) args
        biegunkaArgs = ghcArgs ++ ["-i" ++ view directory script] ++ [script] ++ scriptArgs
    stopBar <- rotateBar logger
    (inh, pid) <- runBiegunkaProcess logger stopBar biegunkaArgs
    _ <- pipe_ (Text.hGetChunk IO.stdin) (Logger.write inh logger . Text.unpack)
    exitcode <- waitForProcess pid
    forOf_ _ExitFailure
           exitcode
           (Logger.write IO.stderr logger . printf "‘%s’ exited with exit code %d\n" script)
    exitWith exitcode

rotateBar :: Logger -> IO (IO ())
rotateBar logger = do
  announceDeath <- newEmptyMVar
  thread <- forkFinally rotation (\_ -> do Logger.write IO.stdout logger "\ESC[1G\ESC[0K"; putMVar announceDeath ())
  return (do killThread thread; readMVar announceDeath)
 where
  rotation = for_ (cycle "/-\\|") (\c -> do Logger.write IO.stdout logger ("\ESC[1D" ++ [c]); threadDelay 80000)

-- | Pipe one 'IO.Handle' into another.
pipe_ :: IO Text -> (Text -> IO ()) -> IO ThreadId
pipe_ = pipe (return ())

-- | Pipe one 'IO.Handle' into another and do _something_ on
-- receiving the first line.
pipe :: IO () -> IO Text -> (Text -> IO ()) -> IO ThreadId
pipe io get put =
  forkIO (flip fix True (\loop first ->
    (do chunk <- get
        unless (Text.null chunk)
               (do when first io
                   put chunk
                   loop False))))
{-# ANN pipe "HLint: ignore Redundant flip" #-}

runBiegunkaProcess :: Logger -> IO () -> [String] -> IO (IO.Handle, ProcessHandle)
runBiegunkaProcess logger stopBar args = do
  (Just inh, Just outh, Just errh, ph) <- createProcess process
  IO.hSetBuffering inh IO.NoBuffering
  _ <- pipe stopBar (Text.hGetChunk outh) (Logger.write IO.stdout logger . Text.unpack)
  _ <- pipe stopBar (Text.hGetChunk errh) (Logger.write IO.stderr logger . Text.unpack)
  return (inh, ph)
 where
  process = (proc "cabal" (["--no-require-sandbox", "exec", "runhaskell", "--"] ++ args))
    { std_in        = CreatePipe
    , std_out       = CreatePipe
    , std_err       = CreatePipe
    , close_fds     = True
    , delegate_ctlc = True
    }

-- | Deeply traverse working directory to find all files named @Biegunka.hs@.
find :: FilePath -> IO [FilePath]
find fp =
  runResourceT . runConduit $
    sourceDirectoryDeep fp =$= CL.filter (elemOf filename scriptName) =$= CL.consume

-- | Traverse directory deeply, ignoring symlinks and directories starting with a dot.
sourceDirectoryDeep
  :: MonadResource m => FilePath -> Producer m FilePath
sourceDirectoryDeep =
  traverseDirectory
 where
  traverseDirectory :: MonadResource m => FilePath -> Producer m FilePath
  traverseDirectory dir = CF.sourceDirectory dir =$= awaitForever go

  go :: MonadResource m => FilePath -> Producer m FilePath
  go fp = do
    status <- liftIO (Posix.getSymbolicLinkStatus fp)
    if | Posix.isRegularFile status -> yield fp
       | Posix.isDirectory status && not ("." `List.isPrefixOf` view filename fp) -> traverseDirectory fp
       | otherwise -> return ()
