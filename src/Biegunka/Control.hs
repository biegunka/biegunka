{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..)
    -- * Common interpreters controls
  , Controls, root, appData, logger
    -- * Generic interpreters
  , pause
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import System.IO

import Control.Lens
import Data.Default
import Data.Semigroup (Semigroup(..), Monoid(..))
import System.Wordexp (wordexp, nosubst, noundef)
import System.Console.Terminfo.PrettyPrint (TermDoc, displayDoc)
import Text.PrettyPrint.Free

import Biegunka.Language
import Biegunka.Transform (fromEL)


-- | Common interpreters controls
data Controls = Controls
  { _root    :: FilePath -- ^ Root path for 'Source' layer
  , _appData :: FilePath -- ^ Biegunka profile files path
  , _logger  :: (TermDoc -> IO ())
  }

makeLensesWith (defaultRules & generateSignatures .~ False) ''Controls


-- | Root path for 'Source' layer lens
root :: Lens' Controls FilePath

-- | Biegunka profile files path
appData :: Lens' Controls FilePath

-- | Logger channel
logger :: Lens' Controls (TermDoc -> IO ())

instance Default Controls where
  def = Controls
    { _root    = "/"
    , _appData = "~/.biegunka"
    , _logger  = const (return ())
    }


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { interpret :: Controls -> [IL] -> IO ()
  }

-- | Two 'Interpreter's combined take the same 'Script' and do things one after another
instance Semigroup Interpreter where
  I f <> I g = I $ \c s -> f c s >> g c s

-- | Empty 'Interpreter' does nothing.
-- Two 'Interpreter's combined take the same 'Script' and do things one after another
instance Monoid Interpreter where
  mempty = I $ \_ _ -> return ()
  mappend = (<>)


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Controls -> Controls) -- ^ User defined settings
         -> Script Profiles       -- ^ Script to interpret
         -> Interpreter           -- ^ Combined interpreters
         -> IO ()
biegunka (($ def) -> c) s (I f) = do
  d <- c ^. root . to expand
  e <- c ^. appData . to expand
  l <- newChan
  forkIO $ loggerThread l
  f (c & root .~ d & appData .~ e & logger .~ (writeChan l)) (fromEL s d)

expand :: String -> IO String
expand x = do
  es <- wordexp (nosubst <> noundef) x
  return $ case es of
    Right [e] -> e
    _         -> x


-- | Simple interpreter example that just waits user to press any key
pause :: Interpreter
pause = I $ \c _ -> view logger c (text "Press any key to continue" <//> line) >> getch
 where
  getch = do
    hSetBuffering stdin NoBuffering
    _ <- getChar
    hSetBuffering stdin LineBuffering


-- | Display supplied docs
loggerThread :: Chan TermDoc -> IO ()
loggerThread c = forever $ readChan c >>= displayDoc 0.6
