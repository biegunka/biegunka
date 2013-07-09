{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), interpret
    -- * Settings common for all interpreters
  , Settings, root, appData, logger, colors, local
    -- * Color scheme controls
  , ColorScheme(..), noColors, actionColor, sourceColor
  , srcColor, dstColor, errorColor, retryColor
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


-- | Settings common for all interpreters and also specific for this one
data Settings a = Settings
  { _root        :: FilePath    -- ^ Root path for 'Source' layer
  , _appData     :: FilePath    -- ^ Biegunka profile files path
  , _logger      :: Doc -> IO () -- ^ Logger channel
  , _colors      :: ColorScheme -- ^ Pretty printing
  , _local       :: a          -- ^ Interpreter specific settings
  }

-- | Colors used in logger
data ColorScheme = ColorScheme
  { _actionColor :: Doc -> Doc
  , _sourceColor :: Doc -> Doc
  , _srcColor    :: Doc -> Doc
  , _dstColor    :: Doc -> Doc
  , _errorColor  :: Doc -> Doc
  , _retryColor  :: Doc -> Doc
  }

instance Default ColorScheme where
  def = ColorScheme
    { _actionColor = green
    , _sourceColor = cyan
    , _srcColor    = yellow
    , _dstColor    = magenta
    , _errorColor  = red
    , _retryColor  = yellow
    }

-- | Disable colors
noColors :: ColorScheme
noColors = ColorScheme
  { _actionColor = id
  , _sourceColor = id
  , _srcColor    = id
  , _dstColor    = id
  , _errorColor  = id
  , _retryColor  = id
  }

makeLensesWith ?? ''ColorScheme $ (defaultRules & generateSignatures .~ False)

-- | Action color
actionColor :: Lens' ColorScheme (Doc -> Doc)

-- | Source color
sourceColor :: Lens' ColorScheme (Doc -> Doc)

-- | Src color
srcColor :: Lens' ColorScheme (Doc -> Doc)

-- | Dst color
dstColor :: Lens' ColorScheme (Doc -> Doc)

-- | Error color
errorColor :: Lens' ColorScheme (Doc -> Doc)

-- | Retry color
retryColor :: Lens' ColorScheme (Doc -> Doc)

makeLensesWith (defaultRules & generateSignatures .~ False) ''Settings

-- | Root path for 'Source' layer
root :: Lens' (Settings a) FilePath

-- | Biegunka profile files
appData :: Lens' (Settings a) FilePath

-- | Logger channel
logger :: Lens' (Settings a) (Doc -> IO ())

-- | Pretty printing
colors :: Lens' (Settings a) ColorScheme

-- | Interpreter controls
local :: Lens (Settings a) (Settings b) a b

instance Default a => Default (Settings a) where
  def = Settings
    { _root    = "~"
    , _appData = "~/.biegunka"
    , _logger  = const (return ())
    , _colors  = def
    , _local   = def
    }


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { runInterpreter :: Settings () -> Free (Term Annotate Sources) () -> IO () -> IO ()
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
interpret :: (Settings () -> Free (Term Annotate Sources) () -> IO ()) -> Interpreter
interpret f = I (\c s k -> f c s >> k)


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Settings () -> Settings ()) -- ^ User defined settings
         -> Interpreter                 -- ^ Combined interpreters
         -> Script Sources ()           -- ^ Script to interpret
         -> IO ()
biegunka (($ def) -> c) (I f) s = do
  r  <- c^.root.to expand
  ad <- c^.appData.to expand
  l <- newTQueueIO
  forkIO $ log l
  f (c & root .~ r & appData .~ ad & logger .~ (atomically . writeTQueue l))
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
