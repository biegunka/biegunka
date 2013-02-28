{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), Task
    -- * Common interpreters controls
  , Controls, root
    -- * Generic interpreters
  , pause
  ) where

import Data.Monoid (Monoid(..))
import System.IO

import Control.Lens
import Data.Default
import System.Wordexp

import Biegunka.Flatten (tasks)
import Biegunka.Language (Script, Layer(..), Command)
import Biegunka.Infect (infect)


-- | Common interpreters controls
data Controls = Controls
  { _root :: FilePath -- ^ Root path for 'Source' layer
  } deriving (Show, Read, Eq, Ord)

makeLensesWith (defaultRules & generateSignatures .~ False) ''Controls


-- | Root path for 'Source' layer lens
root :: Lens' Controls FilePath

instance Default Controls where
  def = Controls
    { _root = "/"
    }


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { interpret :: forall l b. Controls -> [Task l b] -> IO ()
  }

type Task l b = [Command l () b]

-- | Empty 'Interpreter' does nothing. Two 'Interpreter's combined
-- take the same 'Script' and do things one after another
instance Monoid Interpreter where
  mempty = I $ \_ _ -> return ()
  I f `mappend` I g = I $ \c s -> f c s >> g c s


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: Controls        -- ^ Common settings
         -> Script Profiles -- ^ Script to interpret
         -> Interpreter     -- ^ Combined interpreters
         -> IO ()
biegunka c s (I f) = do
  d <- c ^! root . act subst
  let c' = c & root .~ d
  f c' $ map ((c' ^. root) `infect`) (tasks s)
 where
  subst x = do
    es <- wordexp x mempty
    return $ case es of
      []    -> x
      (e:_) -> e


-- | Simple interpreter example that just waits user to press any key
pause :: Interpreter
pause = I $ \_ _ -> putStrLn "Press any key to continue" >> getch
 where
  getch = do
    hSetBuffering stdin NoBuffering
    _ <- getChar
    hSetBuffering stdin LineBuffering
