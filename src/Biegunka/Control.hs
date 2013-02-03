{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Controlling biegunka interpreters and their composition
module Biegunka.Control
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..)
    -- * Common interpreters controls
  , Controls, root
  ) where

import Data.Monoid (Monoid(..))

import Control.Lens
import Control.Monad.Free (Free)
import Data.Default

import Biegunka.Flatten (flatten)
import Biegunka.Language (Script, Layer(..), Command)
import Biegunka.State (infect)


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


-- | Interpreter newtype. Takes a 'Script' and performs some 'IO'
newtype Interpreter = I
  { interpret :: forall a l. Free (Command l ()) a -> IO ()
  }

-- | Empty 'Interpreter' does nothing. Two 'Interpreter's combined
-- take the same 'Script' and do things one after another
instance Monoid Interpreter where
  mempty = I $ \_ -> return ()
  I a `mappend` I b = I $ \s -> a s >> b s


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: Controls       -- ^ Common settings
         -> Script Profile -- ^ Script to interpret
         -> Interpreter    -- ^ Combined interpreters
         -> IO ()
biegunka c s (I i) = i $ (c ^. root) `infect` flatten s
