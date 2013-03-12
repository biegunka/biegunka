{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), Task
    -- * Common interpreters controls
  , Controls, root, appData
    -- * Generic interpreters
  , pause
  ) where

import Data.Monoid (Monoid(..), (<>))
import System.IO

import Control.Lens
import Data.Default
import System.Wordexp (wordexp, nosubst, noundef)

import Biegunka.Preprocess (flatten, infect)
import Biegunka.Language.External (Script, Layer(..), EL)


-- | Common interpreters controls
data Controls = Controls
  { _root    :: FilePath -- ^ Root path for 'Source' layer
  , _appData :: FilePath -- ^ Biegunka profile files path
  } deriving (Show, Read, Eq, Ord)

makeLensesWith (defaultRules & generateSignatures .~ False) ''Controls


-- | Root path for 'Source' layer lens
root :: Lens' Controls FilePath

-- | Biegunka profile files path
appData :: Lens' Controls FilePath

instance Default Controls where
  def = Controls
    { _root = "/"
    , _appData = "~/.biegunka"
    }


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { interpret :: forall l b. Controls -> Task l b -> IO ()
  }

type Task l b = [EL l () b]

-- | Empty 'Interpreter' does nothing. Two 'Interpreter's combined
-- take the same 'Script' and do things one after another
instance Monoid Interpreter where
  mempty = I $ \_ _ -> return ()
  I f `mappend` I g = I $ \c s -> f c s >> g c s


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Controls -> Controls) -- ^ User defined settings
         -> Script Profiles       -- ^ Script to interpret
         -> Interpreter           -- ^ Combined interpreters
         -> IO ()
biegunka (($ def) -> c) s (I f) = do
  d <- c ^. root . to expand
  e <- c ^. appData . to expand
  f (c & root .~ d & appData .~ e) $ (d `infect`) (flatten s)

expand :: String -> IO String
expand x = do
  es <- wordexp (nosubst <> noundef) x
  return $ case es of
    Right [e] -> e
    _         -> x


-- | Simple interpreter example that just waits user to press any key
pause :: Interpreter
pause = I $ \_ _ -> putStrLn "Press any key to continue" >> getch
 where
  getch = do
    hSetBuffering stdin NoBuffering
    _ <- getChar
    hSetBuffering stdin LineBuffering
