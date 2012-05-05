{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Biegunka.Core is module defining interfaces for repositories and script engines.
-- Also it provides a "glue" combining repository layer and script one.
module Biegunka.Core
  ( (-->)
  , Biegunka, Repository(..), ScriptI(..), Script(..), Compiler(..)
  ) where

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Set (Set)

import Biegunka.DB

-- | Repository interface.
-- It should support these operations:
class Repository ρ where
  -- | Clone a repository to the file system
  clone ∷ ρ → IO Bool
  -- | Update an existent repository on the disk
  update ∷ ρ → IO ()
  -- | Get (absolute) path to the repository
  path ∷ ρ → String

-- | Script Engine interface.
-- It should support these operations:
class ScriptI μ where
  -- | Send a message to stdout
  message ∷ String → μ ()
  -- | Link a repository somewhere
  link_repo_itself ∷ FilePath → μ ()
  -- | Link a file within repository somewhere
  link_repo_file ∷ FilePath → FilePath → μ ()
  -- | Copy a file within repository somewhere
  copy_repo_file ∷ FilePath → FilePath → μ ()
  -- | Compile a file within repository with some compiler somewhere
  compile_with ∷ Compiler → FilePath → FilePath → μ ()

-- | Compilers enumeration
data Compiler = GHC -- ^ The Glorious Glasgow Haskell Compilation System

-- | The wrapper to WriterT (for logging installed files) and ReaderT (for saving repository root path) monad transformers. The core of the script engine.
newtype Script α =
  Script { runScript ∷ WriterT (Set FilePath)
                         (ReaderT FilePath IO) α
         } deriving (Monad, MonadIO)

-- | The glue combining repository and script layers.
-- Update current status of defined repository and then run script actions on it.
(-->) ∷ Repository ρ ⇒ IO ρ → Script () → IO Biegunka
mr --> s = mr >>= \r → create (path r) <$> runReaderT (execWriterT $ runScript s) (path r)
