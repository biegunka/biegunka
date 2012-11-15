{-# LANGUAGE DataKinds #-}
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
import Control.Exception (Exception, SomeException, handle, throw)
import Control.Monad ((<=<))
import Control.Monad.Free (Free)
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..))
import Data.Typeable (Typeable)

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>), (<.>))

import Biegunka.DSL (Command(P), foldie)


newtype Biegunka = Biegunka
  { unBiegunka ∷ Map String (Map FilePath (Set FilePath)) } deriving (Show, Eq, Monoid)


newtype Repos = Repos (Map FilePath (Set FilePath))


biegunize ∷ Map String (Map FilePath (Set FilePath)) → Biegunka
biegunize = Biegunka


data AesonFailedToDecode = AesonFailedToDecode deriving (Typeable, Show)


instance Exception AesonFailedToDecode


instance FromJSON Repos where
  parseJSON (Object o) = Repos . M.fromList <$> (mapM repo =<< o .: "repos")
   where
    repo r = do
      n ← r .: "path"
      fs ← r .: "files"
      return (n, S.fromList fs)
  parseJSON _ = empty


instance ToJSON Repos where
  toJSON (Repos α) =
    object ["repos" .= map repoToJSON (M.toList α)]
   where
    repoToJSON (k, v) = object ["path" .= k, "files" .= S.toList v]


load ∷ Free (Command l s) c → IO Biegunka
load = fmap (Biegunka . M.fromList) . mapM readProfile . catMaybes . foldie (:) [] f
 where
  readProfile k = do
    h ← getHomeDirectory
    handle (\(_ ∷ SomeException) → return mempty) $ do
      t ← B.readFile (h </> ".biegunka" <.> k)
      case decode t of
        Just (Repos p) → return (k, p)
        Nothing → throw AesonFailedToDecode

  f (P name _ _) = Just name
  f _ = Nothing


save ∷ Biegunka → IO ()
save (Biegunka x) = getHomeDirectory >>= \hd →
  traverseWithKey_ (\k a → B.writeFile (hd </> ".biegunka" <.> k) (encode (Repos a))) x
 where
  traverseWithKey_ f m = M.traverseWithKey f m >> return ()


filepaths ∷ Biegunka → [FilePath]
filepaths = S.toList <=< M.elems <=< M.elems . unBiegunka


sources ∷ Biegunka → [FilePath]
sources = M.keys <=< M.elems . unBiegunka
