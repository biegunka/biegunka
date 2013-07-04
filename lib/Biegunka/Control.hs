{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), interpret
    -- * Common interpreters controls
  , Controls, root, appData, logger, colors
    -- * Generic interpreters
  , pause, confirm
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless)
import Data.Char (toLower)
import Prelude hiding (log)
import System.IO

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue, isEmptyTQueue)
import           Control.Lens
import           Control.Monad.Free
import           Data.Default
import           Data.Function (fix)
import           Data.Semigroup (Semigroup(..), Monoid(..))
import qualified System.Console.Terminal.Size as Term
import           System.Wordexp (wordexp, nosubst, noundef)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import Biegunka.Language
import Biegunka.Script


-- | Common interpreters controls
data Controls = Controls
  { _root    :: FilePath    -- ^ Root path for 'Source' layer
  , _appData :: FilePath    -- ^ Biegunka profile files path
  , _logger  :: Doc -> IO () -- ^ Logger channel
  , _colors  :: Bool        -- ^ Pretty printing
  }

makeLensesWith (defaultRules & generateSignatures .~ False) ''Controls

-- | Root path for 'Source' layer
root :: Lens' Controls FilePath

-- | Biegunka profile files
appData :: Lens' Controls FilePath

-- | Logger channel
logger :: Lens' Controls (Doc -> IO ())

-- | Pretty printing
colors :: Lens' Controls Bool

instance Default Controls where
  def = Controls
    { _root    = "/"
    , _appData = "~/.biegunka"
    , _logger  = const (return ())
    , _colors  = True
    }


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { runInterpreter :: Controls -> Free (Term Annotate Profiles) () -> IO () -> IO ()
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
interpret :: (Controls -> Free (Term Annotate Profiles) () -> IO ()) -> Interpreter
interpret f = I (\c s k -> f c s >> k)


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Controls -> Controls) -- ^ User defined settings
         -> Interpreter           -- ^ Combined interpreters
         -> Script Profiles ()    -- ^ Script to interpret
         -> IO ()
biegunka (($ def) -> c) (I f) s = do
  r  <- c ^. root . to expand
  ad <- c ^. appData . to expand
  let z = if view colors c then id else plain
  l <- newTQueueIO
  forkIO $ log l
  f (c & root .~ r & appData .~ ad & logger .~ (atomically . writeTQueue l . z))
    (evalScript (def & app .~ r) s)
    (return ())
  fix $ \wait ->
    atomically (isEmptyTQueue l) >>= \e -> unless e (threadDelay 10000 >> wait)


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


-- | Display supplied docs
log :: TQueue Doc -> IO ()
log q = forever $ do
  w <- fmap (maybe 80 Term.width) Term.size
  d <- atomically (readTQueue q)
  displayIO stdout (renderPretty 0.9 w d)
  hFlush stdout
