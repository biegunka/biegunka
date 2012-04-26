{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Biegunka
  ( (-->), bzdury
  , Script(..)
  ) where

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Monoid ((<>), mconcat)
import qualified Data.Map as M

import Biegunka.Repository

newtype Script a =
  Script { runScript ∷ WriterT [FilePath]
                         (ReaderT FilePath IO) a
         } deriving (Monad, MonadIO)

type Biegunka = Map FilePath [FilePath]

(-->) ∷ Repository a ⇒ a → Script () → IO Biegunka
r --> s = do
  cloned ← clone r
  unless cloned $ do
    updated ← update r
    unless updated $
      (error $ "Biegunka: Repo directory " <> path r <> " does exist, but there is some crap!")
  M.singleton (path r) <$> runReaderT (execWriterT $ runScript s) (path r)

bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs
