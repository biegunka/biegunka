{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), interpret
    -- * Generic interpreters
  , pause, confirm
  ) where

import           Control.Monad (forever, unless)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad.Free (Free)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue
  (TQueue, newTQueueIO, readTQueue, writeTQueue, isEmptyTQueue)
import           Data.Char (toLower)
import           Data.Default
import           Data.Semigroup (Semigroup(..), Monoid(..))

import           Control.Biegunka.Language
import           Control.Biegunka.Script
  (Script, Annotate, app, profiles, runScript)
import           Control.Biegunka.Settings
import qualified System.Console.Terminal.Size as Term
import           System.IO
import           System.Wordexp (wordexp, nosubst, noundef)
import           Text.PrettyPrint.ANSI.Leijen
  (Doc, displayIO, renderPretty, (<//>), text, line)


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { unInterpreter
      :: Settings ()
      -> Free (Term Annotate Sources) ()
      -> IO ()
      -> IO ()
  }

-- | Two 'Interpreter's combined take the same 'Script' and do things one after another
instance Semigroup Interpreter where
  I f <> I g = I $ \c s k -> f c s (g c s k)

-- | Empty 'Interpreter' does nothing.
-- Two 'Interpreter's combined take the same 'Script' and do things one after another
instance Monoid Interpreter where
  mempty = I $ \_ _ k -> k
  mappend = (<>)

-- | Interpreter that calls its continuation after interpretation
interpret
  :: (Settings () -> Free (Term Annotate Sources) () -> IO ())
  -> Interpreter
interpret f = I (\c s k -> f c s >> k)

runInterpreter :: Interpreter -> Settings () -> Free (Term Annotate 'Sources) () -> IO ()
runInterpreter (I f) c s = f c s (return ())


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Settings () -> Settings ()) -- ^ User defined settings
         -> Interpreter                 -- ^ Combined interpreters
         -> Script Sources ()           -- ^ Script to interpret
         -> IO ()
biegunka (($ def) -> c) interpreter script = do
  appRoot <- c^.root.to expand
  dataDir <- c^.appData.to expand
  bracket spawnLog waitLog $ \logQueue -> do
    let (annotatedScript, annotations) = runScript def (def & app .~ appRoot) script
        settings = c
          & root    .~ appRoot
          & appData .~ dataDir
          & logger  .~ writeLog logQueue
          & targets .~ annotations^.profiles.to Subset
    runInterpreter interpreter settings annotatedScript

-- | Spawns a thread that reads log queue and
-- pretty prints messages
spawnLog :: IO (TQueue Doc)
spawnLog = newTQueueIO >>= \queue -> forkIO (loop queue) >> return queue where
  loop queue = forever $ do
    width <- fmap (maybe 80 Term.width) Term.size
    doc <- readLog queue
    displayIO stdout (renderPretty 0.9 width doc)
    hFlush stdout
    loop queue

-- | Read a doc from log queue
readLog :: TQueue a -> IO a
readLog = atomically . readTQueue

-- | Write a doc to log queue
writeLog :: TQueue a -> a -> IO ()
writeLog queue = atomically . writeTQueue queue

-- | Loop until log queue is empty
waitLog :: TQueue a -> IO ()
waitLog queue = go where
  go    = atomically (isEmptyTQueue queue) >>= \e -> unless e (threadDelay delay >> go)
  delay = 10000


-- | Take first glob expansion result
expand :: String -> IO String
expand x = do
  es <- wordexp (nosubst <> noundef) x
  return $ case es of
    Right [e] -> e
    _         -> x


-- | Interpreter that just waits user to press any key
pause :: Interpreter
pause = interpret $ \c _ -> view logger c (text "Press any key to continue" <//> line) >> getch
 where
  getch = do
    hSetBuffering stdin NoBuffering
    _ <- getChar
    hSetBuffering stdin LineBuffering

-- | Interpreter that awaits user confirmation
confirm :: Interpreter
confirm = I $ \c _ k -> do
  r <- prompt (view logger c) (text "Proceed? [y/n] ")
  case r of
    True  -> k
    False -> return ()
 where
  prompt l m = do
    l m
    r <- getLine
    case map toLower r of
      "y" -> return True
      "n" -> return False
      _   -> prompt l m
