{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka
  ( (-->), bzdury
  ) where

import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Monoid (mconcat)

import Biegunka.Repository

newtype Biegunka a =
  Biegunka { runBiegunka ∷ WriterT BiegunkaState
                             (ReaderT FilePath IO) a
           } deriving (Monad, MonadIO)

type BiegunkaState = Map String [FilePath]

(-->) ∷ Repository a ⇒ a → Biegunka () → IO BiegunkaState
src --> script = runReaderT (execWriterT (runBiegunka script)) (hash src)

bzdury ∷ [IO BiegunkaState] → IO BiegunkaState
bzdury xs = mconcat <$> sequence xs
