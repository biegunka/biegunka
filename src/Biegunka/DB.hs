{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
-- | Biegunka.DB is module defining operations on the result of installation scripts, Biegunkas.
module Biegunka.DB
  ( Biegunka
  , create, load, save, merge, delete, purge, wipe
  , pretty
  , bzdury
  ) where

import Data.Functor ((<$>))
import Control.Arrow ((***))
import Control.Monad (when)
import Data.Map (Map, (!))
import Data.Monoid (Monoid(..))
import Data.Set (Set)
import System.Directory (getHomeDirectory, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.Set as S

-- | The result of the installation script.
-- Contains a map of repository paths (keys) to installed files (values)
newtype Biegunka =
  Biegunka { φ ∷ Map FilePath
                       (Set FilePath)
           } deriving (Monoid, Show)

-- | Combine all Biegunkas into one.
bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs

-- | Create an Biegunka for a repostory.
create ∷ FilePath → Set FilePath → Biegunka
-- | Load a Biegunka from ~/.biegunka.db.
-- If ~/.biegunka.db doesn't exist return an empty map
load ∷ IO Biegunka
-- | Save a Biegunka to ~/.biegunka.db (warning: overwrite the old one).
save ∷ Biegunka → IO ()
-- | Merge (sum) two Biegunkas.
merge ∷ Biegunka → Biegunka → Biegunka
-- | Delete an installed file related to specified repository.
delete ∷ Biegunka → FilePath → FilePath → IO Biegunka
-- | Purge all installed files related to specified repository.
purge ∷ Biegunka → FilePath → IO Biegunka
-- | Wipe all repositories.
wipe ∷ Biegunka → IO ()

create fp = Biegunka . M.singleton fp

load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  exists ← doesFileExist db
  if exists
    then Biegunka . read <$> readFile db
    else return mempty

save (φ → α) = do
  hd ← getHomeDirectory
  α `seq` writeFile (hd </> ".biegunka.db") (show α)

merge (φ → α) (φ → β) = Biegunka $ M.unionWith mappend α β

-- Algorithm is as follows:
-- 1. adjust current map by key `rp' with set with deleted element `fp'
--    note: both Data.Map.adjust and Data.Set.delete return the same data struct on fail
-- 2. if adjusted map is not the same with initial (which means Data.Map.adjust and
--    Data.Set.delete both have succeed) remove file `fp`
-- 3. return new map (either it's old one or really adjusted)
delete (φ → o) rp fp = do
  let n = M.adjust (S.delete fp) rp o
  when (n /= o) $ removeFile fp
  return $ Biegunka n

-- Algorithm is as follows:
-- 1. delete from current map by key `rp'
--    note: Data.Map.delete returns the same data struct on fail
-- 2. if adjusted map is not the same with initial (which means Data.Map.delete
--    has succeed) remove all files by key `rp` in the initial map
-- 3. return new map (either it's old one or really adjusted)
purge (φ → o) rp = do
  let n = M.delete rp o
      xs = S.toList $ o ! rp
  when (n /= o) $
    do mapM_ removeFile xs
       removeDirectoryRecursive rp
  return $ Biegunka n

-- Algorithm is as follows:
-- 1. fold current map to list of files
-- 2. remove every file in the list
-- 3. no point to return something: resulting map is merely the Data.Map.empty one
wipe (φ → db) =
  do let xs = S.toList $ M.fold mappend S.empty db
         ys = M.keys db
     mapM_ removeFile xs
     mapM_ removeDirectoryRecursive ys

-- | Pretty printer for Biegunka.
pretty ∷ Biegunka → String
pretty = render . vcat . map (uncurry ($$) . (text *** vcat . map (nest 2 . text) . S.toList)) . M.toList . φ
