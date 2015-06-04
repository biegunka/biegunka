{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
#ifndef TEST
module Generate (scriptFor) where
#else
module Generate where
#endif

import           Control.Lens hiding ((<.>))
import           Control.Monad.Trans.Writer (WriterT, execWriter, tell)
import           Data.Char (toUpper)
import           Data.Foldable (for_, toList)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.FilePath (makeRelative)
import           System.IO (hFlush, stdout)

import           Control.Biegunka.Biegunka (expandHome)
import           Control.Biegunka.Namespace
  (NamespaceRecord(..), SourceRecord(..), FileRecord(..), namespaces, namespacing, withDb)
import           Control.Biegunka.Settings (defaultSettings, biegunkaRoot)


scriptFor :: FilePath -> FilePath -> IO ()
scriptFor rrpat brpat = do
  rr <- expandHome rrpat
  br <- expandHome brpat
  withDb (set biegunkaRoot br defaultSettings) $ \db -> do
    let nss    = view (namespaces.namespacing) db
        types  = uniqueSourcesTypes nss
        script = execWriter (gen nss rr types)
    T.putStr script
    hFlush stdout
 where
  gen db root sources = do
    tell (boilerplate sources)
    ifor_ db $ \nsName (NR nsData) -> do
      write $ namespace nsName
      if M.null nsData
        then write $ indent sourceIndent emptyScript
        else
          ifor_ nsData $ \sourceRecord fileRecords -> do
            write $ source root sourceRecord
            if S.null fileRecords
              then write $ indent fileIndent emptyScript
              else
                for_ fileRecords $
                  tell . line . file root (sourcePath sourceRecord) (sourceOwner sourceRecord)

write :: Monad m => Text -> WriterT Text m ()
write = tell . line

uniqueSourcesTypes :: M.Map String NamespaceRecord -> Set String
uniqueSourcesTypes =
  execWriter . traverse (traverse (tell . S.singleton . sourceType) . M.keys . unGR)


boilerplate :: Set String -> Text
boilerplate sourceTypes = header <> sourceImports sourceTypes <> main

header :: Text
header = T.unlines
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE TemplateHaskell #-}"
  , "module Main (main) where"
  , ""
  , "import Control.Biegunka"
  , "import Data.Default (def)"
  ]

sourceImports :: Set String -> Text
sourceImports =
  T.unlines . map (\ty -> "import Control.Biegunka.Source." <> string (capitalize ty)) . toList

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""

main :: Text
main = T.unlines
  [ ""
  , ""
  , "data Environments = Default"
  , ""
  , "biegunkaOptions ''Environments"
  , ""
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  (env, runBiegunka) <- options"
  , "  case env of"
  , "    Default -> runBiegunka def script"
  , ""
  , "script :: Script Sources ()"
  , "script = do"
  ]


type User = Maybe (Either String Int)

namespace :: String -> Text
namespace = indent namespaceIndent . go where
  go name = T.unwords ["namespace", string (show name), "$ do"]

source :: FilePath -> SourceRecord -> Text
source appRoot = indent sourceIndent . go where
  go SR { sourceType, fromLocation, sourcePath, sourceOwner } =
    sudoWith Nothing sourceOwner <> T.unwords
      [ string sourceType
      , shown fromLocation
      , shown (makeRelative appRoot sourcePath)
      , "$ do"
      ]

file :: FilePath -> FilePath -> User -> FileRecord -> Text
file appRoot sourceRoot sourceOwner = indent fileIndent . go where
  go FR { fileType, fromSource, filePath, fileOwner } =
    sudoWith sourceOwner fileOwner <> T.unwords
      [ string (action fileType)
      , shown (makeRelative sourceRoot fromSource)
      , shown (makeRelative appRoot filePath)
      ]

  action "template" = "substitute"
  action fileType   = fileType

namespaceIndent, sourceIndent, fileIndent :: Int
namespaceIndent = 2
sourceIndent    = 4
fileIndent      = 6

indent :: Integral n => n -> Text -> Text
indent n message = T.replicate (fromIntegral n) " " <> message

sudoWith :: User -> User -> Text
sudoWith outerOwner user
  | outerOwner == user = ""
  | otherwise          = T.unwords ["sudo", go user, "$ "]
 where
  go Nothing                = ""
  go (Just (Left username)) = shown username
  go (Just (Right userid))  = shown userid

line :: Text -> Text
line = (<> "\n")

string :: String -> Text
string = T.pack

shown :: Show a => a -> Text
shown  = string . show

emptyScript :: Text
emptyScript = "return ()"
