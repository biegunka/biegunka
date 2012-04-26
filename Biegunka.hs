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

import Biegunka.Repository

newtype Biegunka a =
  Biegunka { runBiegunka ∷ WriterT BiegunkaState
                             (ReaderT FilePath IO) a
           } deriving (Monad, MonadIO)

type BiegunkaState = Map String [FilePath]

(-->) ∷ Repository a ⇒ a → Biegunka () → IO BiegunkaState
src --> script = do
  cloned ← clone src
  unless cloned $ do
    updated ← update src
    unless updated $
      (error $ "Biegunka: Repo directory " <> path src <> " does exist, but there is some crap!")
  runReaderT (execWriterT (runBiegunka script)) (path src)

bzdury ∷ [IO BiegunkaState] → IO BiegunkaState
bzdury xs = mconcat <$> sequence xs
