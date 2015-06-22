{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Json
  ( out
  ) where

import           Control.Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode as Aeson
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Text

import           Control.Biegunka.Biegunka (expandHome)
import           Control.Biegunka.Namespace
  (Db, Namespaces(..), NamespaceRecord(..), SourceRecord(..), FileRecord(..), namespaces, withDb)
import           Control.Biegunka.Settings (defaultSettings, biegunkaRoot)


out :: FilePath -> IO ()
out brpat = do
  br <- expandHome brpat
  withDb (set biegunkaRoot br defaultSettings) $
    Text.putStrLn . Builder.toLazyText . toBuilder

toBuilder :: Db -> Builder
toBuilder (view namespaces -> Namespaces { _unNamespaces }) =
  Aeson.encodeToTextBuilder
    (Aeson.object
      [ "namespaces" Aeson..= ns (Map.toList _unNamespaces)
      ])
 where
  ns xs = Aeson.object (map (\(k, v) -> fromString k Aeson..= nr v) xs)
  nr (NR t) = Aeson.object ["sources" Aeson..= map repo (Map.toList t)]
   where
    repo (k, v) =
      Aeson.object [ "info"  Aeson..= sr k
                   , "files" Aeson..= map fr (Set.toList v)]
  sr SR { sourceType, fromLocation, sourcePath, sourceOwner } = Aeson.object
    [ "type" Aeson..= sourceType
    , "from" Aeson..= fromLocation
    , "path" Aeson..= sourcePath
    , "user" Aeson..= who sourceOwner
    ]
  fr FR { fileType, fromSource, filePath, fileOwner } = Aeson.object
    [ "type" Aeson..= fileType
    , "from" Aeson..= fromSource
    , "path" Aeson..= filePath
    , "user" Aeson..= who fileOwner
    ]

who :: Maybe (Either String Int) -> String
who = either id show . fromMaybe (Left "(unknown)")
