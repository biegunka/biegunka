{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Biegunka.Core
  ( (-->), bzdury
  , Biegunka, Script(..), Repository(..)
  , save, load
  ) where

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Monoid ((<>), mconcat)
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Map as M

class Repository a where
  clone ∷ a → IO Bool
  update ∷ a → IO Bool
  path ∷ a → String

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

save ∷ Biegunka → IO ()
save new = do
  !old ← load
  hd ← getHomeDirectory
  writeFile (hd </> ".biegunka.db") (show $ old <> new)

load ∷ IO Biegunka
load = do
  hd ← getHomeDirectory
  let db = (hd </> ".biegunka.db")
  exists ← doesFileExist db
  if exists
    then read <$> readFile db
    else return M.empty
