-- | Run (or check) biegunka script
module Run (run) where

import           Control.Concurrent (ThreadId, forkIO, forkFinally, threadDelay, killThread)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Control.Lens hiding ((<.>))
import           Control.Monad (when, unless)
import           Data.Foldable (for_)
import           Data.Function (fix)
import           Data.List (isPrefixOf, partition)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           System.Exit (ExitCode(..), exitSuccess, exitWith)
import           System.FilePath.Lens (directory)
import           System.Process
import qualified System.IO as IO


-- | Runs (or checks) biegunka script.
--
-- Does a couple of smart things:
--
--   * Uses @cabal exec@ to run @runhaskell@ inside the cabal sandbox automatically
--
--   * If script path argument is a directory, then default script name is
--   automatically appended, e.g. @biegunka\/@ becomes @biegunka\/Biegunka.hs@
--
--   * Script path directory name is added to paths where ghc searches for
--   modules (@-i@ option)
run :: [String] -> FilePath -> IO ()
run args target = do
  let (biegunkaArgs, ghcArgs) = partition ("--" `isPrefixOf`) args
  stopBar <- rotateBar
  (inh, pid) <- runBiegunkaProcess
    stopBar
    (ghcArgs ++ ["-i" ++ view directory target] ++ [target] ++ biegunkaArgs)
  _ <- pipe_ IO.stdin inh
  exitcode <- waitForProcess pid
  exit exitcode
 where
  exit ExitSuccess =
    exitSuccess
  exit (ExitFailure s) = do
    putStrLn ("Biegunka script exited with exit code " ++ show s)
    exitWith (ExitFailure s)

rotateBar :: IO (IO ())
rotateBar = do
  announceDeath <- newEmptyMVar
  thread <- forkFinally rotation (\_ -> do putStr "\r \r"; putMVar announceDeath ())
  return (do killThread thread; readMVar announceDeath)
 where
  rotation = for_ (cycle "/-\\|/-\\|") (\c -> do putChar '\r'; putChar c; threadDelay 80000)

-- | Pipe one 'IO.Handle' into another.
pipe_ :: IO.Handle -> IO.Handle -> IO ThreadId
pipe_ = pipe (return ())

-- | Pipe one 'IO.Handle' into another and do _something_ on
-- receiving the first line.
pipe :: IO () -> IO.Handle -> IO.Handle -> IO ThreadId
pipe io inh outh =
  forkIO (flip fix True (\loop first ->
    (do chunk <- Text.hGetChunk inh
        unless (Text.null chunk)
               (do when first io
                   Text.hPutStr outh chunk
                   IO.hFlush outh
                   loop False))))
{-# ANN pipe "HLint: ignore Redundant flip" #-}

runBiegunkaProcess :: IO () -> [String] -> IO (IO.Handle, ProcessHandle)
runBiegunkaProcess stopBar args = do
  (Just inh, Just outh, Just errh, ph) <- createProcess process
  IO.hSetBuffering inh IO.NoBuffering
  _ <- pipe stopBar outh IO.stdout
  _ <- pipe stopBar errh IO.stderr
  return (inh, ph)
 where
  process = CreateProcess
    { cmdspec       = RawCommand "cabal" (["--no-require-sandbox", "exec", "runhaskell", "--"] ++ args)
    , cwd           = Nothing
    , env           = Nothing
    , std_in        = CreatePipe
    , std_out       = CreatePipe
    , std_err       = CreatePipe
    , close_fds     = True
    , create_group  = False
    , delegate_ctlc = True
    }
