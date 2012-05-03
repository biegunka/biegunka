{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.DB
  ( Biegunka
  , create, load, save, merge, delete, wipe
  , bzdury
  ) where

import Data.Functor ((<$>))
import Control.Monad (when)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid, (<>), mconcat)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))
import qualified Data.Map as M

newtype Biegunka =
  Biegunka { φ ∷ Map FilePath
                       [FilePath]
           } deriving Monoid

create ∷ FilePath → [FilePath] → Biegunka
load ∷ IO Biegunka
save ∷ Biegunka → IO ()
merge ∷ Biegunka → Biegunka → Biegunka
delete ∷ Biegunka → FilePath → IO Biegunka
wipe ∷ Biegunka → IO ()

create fp = Biegunka . M.singleton fp

load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  exists ← doesFileExist db
  if exists
    then (Biegunka . read) <$> readFile db
    else (return . Biegunka) M.empty

save (φ → α) = do
  (φ → !β) ← load
  hd ← getHomeDirectory
  writeFile (hd </> ".biegunka.db") (show $ α <> β)

merge (φ → α) (φ → β) = Biegunka $ M.unionWith (<>) α β

delete (φ → db) fp = do
  let r = M.lookup fp db
  when (isJust r) $ mapM_ removeFile (fromJust r)
  return . Biegunka $ M.delete fp db

wipe (φ → db) = mapM_ removeFile (M.toList db >>= snd)

bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs
