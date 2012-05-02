{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Biegunka.Core
  ( (-->), bzdury
  , Biegunka, Script(..), ScriptI(..), Repository(..)
  , save, load, delete
  ) where

import Control.Monad (when)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.Writer (WriterT(..), execWriterT)
import Data.Functor ((<$>))
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>), mconcat)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))
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

save ∷ Biegunka → IO ()
save new = do
  !old ← load
  hd ← getHomeDirectory
  writeFile (hd </> ".biegunka.db") (show $ old <> new)

load ∷ IO Biegunka
load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  exists ← doesFileExist db
  if exists
    then read <$> readFile db
    else return M.empty

delete ∷ Biegunka → FilePath → IO Biegunka
delete db fp = do
  let r = M.lookup fp db
  when (isJust r) $ mapM_ removeFile (fromJust r)
  return $ M.delete fp db
