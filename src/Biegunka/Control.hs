{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..)
    -- * Common interpreters controls
  , Controls, root, appData, logger, colors
    -- * Generic interpreters
  , pause
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue, isEmptyTQueue)
import Control.Monad (forever, mplus, unless)
import System.IO

import           Control.Lens
import           Data.Default
import           Data.Function (fix)
import           Data.Semigroup (Semigroup(..), Monoid(..))
import qualified System.Console.Terminal.Size as Term
import           System.Wordexp (wordexp, nosubst, noundef)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>))

import           Biegunka.Language
import           Biegunka.Transform (fromEL)


-- | Common interpreters controls
data Controls = Controls
  { _root    :: FilePath -- ^ Root path for 'Source' layer
  , _appData :: FilePath -- ^ Biegunka profile files path
  , _logger  :: Doc -> IO ()
  , _colors  :: Bool
  }

makeLensesWith (defaultRules & generateSignatures .~ False) ''Controls

-- | Root path for 'Source' layer lens
root :: Lens' Controls FilePath

-- | Biegunka profile files path
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
         -> Interpreter           -- ^ Combined interpreters
         -> Script Profiles ()    -- ^ Script to interpret
         -> IO ()
biegunka (($ def) -> c) (I f) s = do
  r  <- c ^. root . to expand
  ad <- c ^. appData . to expand
  let z = if view colors c then id else plain
  l <- newTQueueIO
  forkIO $ loggerThread l
  f (c & root .~ r & appData .~ ad & logger .~ (atomically . writeTQueue l . z)) (fromEL s r)
  fix $ \wait ->
    atomically (isEmptyTQueue l) >>= \e -> unless e (threadDelay 10000 >> wait)

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
loggerThread :: TQueue Doc -> IO ()
loggerThread queue = forever $ do
  w <- fmap Term.width Term.size `mplus` return 80
  atomically (readTQueue queue) >>= displayIO stdout . renderPretty 0.9 w >> hFlush stdout
