{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Biegunka
  ( (-->), bzdury
  , Biegunka(..)
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

newtype Biegunka a =
  Biegunka { runBiegunka ∷ WriterT [FilePath]
                             (ReaderT FilePath IO) a
           } deriving (Monad, MonadIO)

type Wymioty = Map FilePath [FilePath]

(-->) ∷ Repository a ⇒ a → Biegunka () → IO Wymioty
src --> script = do
  cloned ← clone src
  unless cloned $ do
    updated ← update src
    unless updated $
      (error $ "Biegunka: Repo directory " <> path src <> " does exist, but there is some crap!")
  M.singleton (path src) <$> runReaderT (execWriterT (runBiegunka script)) (path src)

bzdury ∷ [IO Wymioty] → IO Wymioty
bzdury xs = mconcat <$> sequence xs
