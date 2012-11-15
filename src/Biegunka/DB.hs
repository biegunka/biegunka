{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.DB
  ( Biegunka, biegunize
  , load, save
  , filepaths, sources
  ) where

import Control.Applicative ((<$>), empty)
import Control.Monad ((<=<))
import Data.Monoid (Monoid(..))
import Control.Exception (Exception, SomeException, handle, throw)
import Data.Typeable (Typeable)
import System.IO (IOMode(ReadMode), withFile)

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))


newtype Biegunka = Biegunka
  { unBiegunka ∷ Map String (Map FilePath (Set FilePath)) } deriving (Show, Eq, Monoid)


biegunize ∷ Map String (Map FilePath (Set FilePath)) → Biegunka
biegunize = Biegunka


data AesonFailedToDecode = AesonFailedToDecode deriving (Typeable, Show)


instance Exception AesonFailedToDecode


instance FromJSON Biegunka where
  parseJSON (Object o) = Biegunka . M.fromList <$> (mapM profile =<< o .: "profiles")
   where
    profile p = do
      n ← p .: "profile"
      rs ← mapM repo =<< p .: "repos"
      return (n, M.fromList rs)
    repo r = do
      n ← r .: "path"
      fs ← r .: "files"
      return (n, S.fromList fs)
  parseJSON _ = empty


instance ToJSON Biegunka where
  toJSON (Biegunka α) =
    let profileToJSON (k, v) = object ["profile" .= k, "repos" .= map repoToJSON (M.toList v)]
        repoToJSON (k, v) = object ["path" .= k, "files" .= S.toList v]
    in object $ ["profiles" .= map profileToJSON (M.toList α)]


load ∷ IO Biegunka
load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  handle (\(_ ∷ SomeException) → return mempty) $
    withFile db ReadMode $ \h → do
      !j ← B.hGetContents h
      case decode j of
        Just biegunka → return biegunka
        Nothing → throw AesonFailedToDecode


save ∷ Biegunka → IO ()
save α = getHomeDirectory >>= \hd →
  B.writeFile (hd </> ".biegunka.db") (encode α)


filepaths ∷ Biegunka → [FilePath]
filepaths = S.toList <=< M.elems <=< M.elems . unBiegunka


sources ∷ Biegunka → [FilePath]
sources = M.keys <=< M.elems . unBiegunka
