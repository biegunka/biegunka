{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Biegunka.DB is module defining operations on the result of installation scripts, Biegunkas.
module Biegunka.DB
  ( Biegunka
  , create, load, save, merge, delete, purge, wipe
  , pretty
  , bzdury
  ) where

import Control.Applicative ((<$>), empty, liftA2)
import Control.Arrow ((***))
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import System.IO (IOMode(ReadMode), withFile)

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory (getHomeDirectory, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Text.PrettyPrint hiding (empty)


-- | The result of the installation script.
-- Contains a map of repository paths (keys) to installed files (values)
newtype Biegunka =
  Biegunka { φ ∷ Map FilePath (Set FilePath) } deriving (Monoid, Show)


instance FromJSON Biegunka where
  parseJSON (Object o) = Biegunka . M.fromList <$>
    (o .: "repo" >>= mapM (\r → liftA2 (,) (r .: "path") (S.fromList <$> r .: "files")))
  parseJSON _ = empty
instance ToJSON Biegunka where
  toJSON (Biegunka α) = object [ "repo" .= map (\(k,v) → object ["path" .= k, "files" .= S.toList v]) (M.toList α) ]


-- | Combine all Biegunkas into one.
bzdury ∷ [IO Biegunka] → IO Biegunka
bzdury xs = mconcat <$> sequence xs


-- | Create an Biegunka for a repostory.
create ∷ FilePath → Set FilePath → Biegunka
create fp = Biegunka . M.singleton fp


-- | Load a Biegunka from ~/.biegunka.db.
-- If ~/.biegunka.db doesn't exist return an empty map
load ∷ IO Biegunka
load =
  do db ← (</> ".biegunka.db") <$> getHomeDirectory
     exists ← doesFileExist db
     if exists
       then withFile db ReadMode $ \h →
              do !j ← B.hGetContents h
                 return $ fromMaybe mempty (decode j ∷  Maybe Biegunka)
       else return mempty


-- | Save a Biegunka to ~/.biegunka.db (warning: overwrite the old one).
save ∷ Biegunka → IO ()
save α =
  do hd ← getHomeDirectory
     B.writeFile (hd </> ".biegunka.db") (encode α)


-- | Merge (sum) two Biegunkas.
merge ∷ Biegunka → Biegunka → Biegunka
merge (φ → α) (φ → β) = Biegunka $ M.unionWith mappend α β


-- | Delete an installed file related to specified repository.
--
-- Algorithm is as follows:
-- 1. adjust current map by key `rp' with set with deleted element `fp'
--    note: both Data.Map.adjust and Data.Set.delete return the same data struct on fail
-- 2. if adjusted map is not the same with initial (which means Data.Map.adjust and
--    Data.Set.delete both have succeed) remove file `fp`
-- 3. return new map (either it's old one or really adjusted)
delete ∷ Biegunka → FilePath → FilePath → IO Biegunka
delete (φ → o) rp fp =
  do let n = M.adjust (S.delete fp) rp o
     when (n /= o) $ removeFile fp
     return $ Biegunka n


-- | Purge all installed files related to specified repository.
--
-- Algorithm is as follows:
-- 1. delete from current map by key `rp'
--    note: Data.Map.delete returns the same data struct on fail
-- 2. if adjusted map is not the same with initial (which means Data.Map.delete
--    has succeed) remove all files by key `rp` in the initial map
-- 3. return new map (either it's old one or really adjusted)
purge ∷ Biegunka → FilePath → IO Biegunka
purge (φ → o) rp =
  do let n = M.delete rp o
         xs = S.toList $ o ! rp
     when (n /= o) $
       do mapM_ removeFile xs
          removeDirectoryRecursive rp
     return $ Biegunka n


-- | Wipe all repositories.
--
-- Algorithm is as follows:
-- 1. fold current map to list of files
-- 2. remove every file in the list
-- 3. no point to return something: resulting map is merely the Data.Map.empty one
wipe ∷ Biegunka → IO ()
wipe (φ → db) =
  do let xs = S.toList $ M.fold mappend S.empty db
         ys = M.keys db
     mapM_ removeFile xs
     mapM_ removeDirectoryRecursive ys

-- | Pretty printer for Biegunka.
pretty ∷ Biegunka → String
pretty = render . vcat . map (uncurry ($$) . (text *** vcat . map (nest 2 . text) . S.toList)) . M.toList . φ
