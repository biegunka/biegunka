{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Biegunka.DB is module defining operations on the result of installation scripts, Biegunkas.
module Biegunka.DB
  ( Biegunka(..)
  , withBiegunka
  , create, merge
  , removeFile, removeRepo, removeProfile, wipe
  ) where

import Control.Applicative ((<$>), empty, liftA2)
import Control.Monad (when)
import Data.Foldable (fold, foldMap, mapM_)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))
import Data.String (fromString)
import Prelude hiding (mapM_)
import System.IO (IOMode(ReadMode), withFile)

import Data.Aeson hiding (encode)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as B
import System.Directory (getHomeDirectory, doesFileExist, removeDirectoryRecursive)
import qualified System.Directory as D
import System.FilePath ((</>))
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S


-- | The result of the installation script.
-- Contains a map of repository paths (keys) to installed files (values)
newtype Biegunka =
  Biegunka (Map String (Map FilePath (Set FilePath))) deriving (Show, Monoid)


instance FromJSON Biegunka where
  parseJSON (Object o) = Biegunka . M.singleton "default" . M.fromList <$>
    (o .: "default" >>= mapM (\r → liftA2 (,) (r .: "path") (S.fromList <$> r .: "files")))
  parseJSON _ = empty
instance ToJSON Biegunka where
  toJSON (Biegunka α) = object $ map (\(k,v) → (fromString k) .= map (\(k',v') → object ["path" .= k', "files" .= S.toList v']) (M.toList v)) (M.toList α)


-- | Create an Biegunka for a repostory.
create ∷ FilePath → Set FilePath → Map FilePath (Set FilePath)
create fp = M.singleton fp


-- | Load a Biegunka from ~/.biegunka.db, do any actions with it, save it back.
withBiegunka ∷ (Biegunka → IO Biegunka) → IO ()
withBiegunka action = load >>= action >>= save
 where
  load =
    do db ← (</> ".biegunka.db") <$> getHomeDirectory
       exists ← doesFileExist db
       if exists
         then withFile db ReadMode $ \h → do
                !j ← B.hGetContents h
                return $ fromMaybe mempty (decode j ∷ Maybe Biegunka)
         else return mempty

  save α = getHomeDirectory >>= \hd →
    B.writeFile (hd </> ".biegunka.db") (encode EncodingEnv { indentationStep = 2 } α)


-- | Merge two Biegunkas.
merge ∷ Biegunka → Biegunka → Biegunka
merge (Biegunka α) (Biegunka β) =
  let γ = flip M.mapWithKey α $ \k a →
        case M.lookup k β of
          Just profileFiles → M.unionWith mappend a profileFiles
          Nothing → a
      ε = γ `mappend` (β M.\\ α)
  in Biegunka ε



-- | Delete an installed file related to specified repository in given profile.
--
-- Algorithm is as follows:
-- 1. Check if given profile is known
-- 2. If it is then adjust current map by key `rp' with set with deleted element `fp'
--    note: both Data.Map.adjust and Data.Set.delete return the unchanged data struct on fail
-- 3. If adjusted map is changed (which means Data.Map.adjust and
--    Data.Set.delete both have succeed) remove file `fp'
-- 4. Update map in given profile
removeFile ∷ String → FilePath → FilePath → Biegunka → IO Biegunka
removeFile profile repo filepath b@(Biegunka current) =
  case M.lookup profile current of
    Just profileFiles →
      do let adjustedProfileFiles = M.adjust (S.delete filepath) repo profileFiles
         when (adjustedProfileFiles /= profileFiles) $ D.removeFile filepath
         return $ Biegunka $ M.insert profile adjustedProfileFiles current
    Nothing → return b


-- | Remove all installed files related to specified repository.
--
-- Algorithm is as follows:
-- 1. Check if given profile is known
-- 2. If it is then delete from current map by key `rp'
--    note: Data.Map.delete returns unchanged data struct on fail
-- 2. If adjusted map is changed (which means Data.Map.delete
--    has succeed) remove all files by key `rp' in the initial map
-- 4. Update map in given profile
removeRepo ∷ String → FilePath → Biegunka → IO Biegunka
removeRepo profile repo b@(Biegunka current) =
  case M.lookup profile current of
    Just profileFiles →
      do let adjustedProfileFiles = M.delete repo profileFiles
         when (adjustedProfileFiles /= profileFiles) $
           do mapM_ D.removeFile $ profileFiles ! repo
              removeDirectoryRecursive repo
         return $ Biegunka $ M.insert profile adjustedProfileFiles current
    Nothing → return b


-- | Remove all installed repositories related to specified profile.
--
-- Algorithm is as follows:
-- 1. Check if given profile is known
-- 2. If it is then delete from current map by key `rp'
--    note: Data.Map.delete returns unchanged data struct on fail
-- 2. If adjusted map is changed (which means Data.Map.delete
--    has succeed) remove all files by key `rp' in the initial map
-- 4. Update map in given profile
removeProfile ∷ String → Biegunka → IO Biegunka
removeProfile profile b@(Biegunka current) =
  case M.lookup profile current of
    Just profileFiles →
      do mapM_ D.removeFile $ fold profileFiles
         mapM_ removeDirectoryRecursive $ M.keysSet profileFiles
         return $ Biegunka $ M.delete profile current
    Nothing → return b


-- | Wipe all repositories.
--
-- Algorithm is as follows:
-- 1. Fold current map to set of files
-- 2. Remove every file in the set
-- 3. No point to return something: resulting map is merely the Data.Map.empty one
wipe ∷ Biegunka → IO Biegunka
wipe (Biegunka current) =
  do mapM_ D.removeFile $ foldMap fold current
     mapM_ removeDirectoryRecursive $ foldMap M.keysSet current
     return $ Biegunka mempty
