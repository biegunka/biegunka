{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Biegunka.Core
  ( (-->), bzdury
  , Biegunka, Script(..), ScriptI(..), Repository(..)
  ) where

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Monoid (mconcat)
import qualified Data.Map as M

class Repository ρ where
  clone ∷ ρ → IO Bool
  update ∷ ρ → IO Bool
  path ∷ ρ → String

class ScriptI μ where
  message ∷ String → μ ()
  link_repo_itself ∷ FilePath → μ ()
  link_repo_file ∷ FilePath → FilePath → μ ()
  copy_repo_file ∷ FilePath → FilePath → μ ()

newtype Script α =
  Script { runScript ∷ WriterT [FilePath]
                         (ReaderT FilePath IO) α
         } deriving (Monad, MonadIO)

type Biegunka = Map FilePath [FilePath]

(-->) ∷ Repository ρ ⇒ IO ρ → Script () → IO Biegunka
mr --> s = mr >>= \r → M.singleton (path r) <$> runReaderT (execWriterT $ runScript s) (path r)

bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs
