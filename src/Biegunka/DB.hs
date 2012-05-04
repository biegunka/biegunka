{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module Biegunka.DB
  ( Biegunka
  , create, load, save, merge, delete, purge, wipe
  , bzdury
  ) where

import Data.Functor ((<$>))
import Control.Monad (when)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid (Monoid, (<>), mconcat, mempty)
import Data.Set (Set)
import System.Directory (getHomeDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))
import qualified Data.Map as M
import qualified Data.Set as S

newtype Biegunka =
  Biegunka { φ ∷ Map FilePath
                       (Set FilePath)
           } deriving (Monoid, Show)

create ∷ FilePath → Set FilePath → Biegunka
load ∷ IO Biegunka
save ∷ Biegunka → IO ()
merge ∷ Biegunka → Biegunka → Biegunka
delete ∷ Biegunka → FilePath → FilePath → IO Biegunka
purge ∷ Biegunka → FilePath → IO Biegunka
wipe ∷ Biegunka → IO ()

create fp = Biegunka . M.singleton fp

load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  exists ← doesFileExist db
  if exists
    then Biegunka . read <$> readFile db
    else return mempty

save (φ → !α) = do
  hd ← getHomeDirectory
  writeFile (hd </> ".biegunka.db") (show α)

merge (φ → α) (φ → β) = Biegunka $ M.unionWith (<>) α β

delete (φ → o) rp fp = do
  let n = M.adjust (S.delete fp) rp o
  when (n /= o) $ removeFile fp
  return $ Biegunka n

purge (φ → db) fp = do
  let r = S.toList <$> M.lookup fp db
  when (isJust r) $ mapM_ removeFile (fromJust r)
  return . Biegunka $ M.delete fp db

wipe (φ → db) = mapM_ removeFile . S.toList $ M.foldl (<>) S.empty db

bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs
