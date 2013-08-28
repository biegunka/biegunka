{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate (scriptFor) where

import           Control.Lens hiding ((<.>))
import           Control.Monad.Trans.Writer (WriterT, execWriter, tell)
import           Data.Char (toUpper)
import           Data.Default (def)
import           Data.Foldable (Foldable, for_, toList)
import qualified Data.Map as M
import           Data.Monoid ((<>))
import qualified Data.Set as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           System.FilePath (makeRelative)
import           System.IO (hFlush, hPutStrLn, stderr, stdout)
import           System.Wordexp (wordexp, nosubst, noundef)

import           Control.Biegunka.Groups
  (GroupRecord(..), SourceRecord(..), FileRecord(..), these, groups, open)
import           Control.Biegunka.Settings
  (appData, Targets(..), targets)


scriptFor :: FilePath -> FilePath -> [String] -> IO ()
scriptFor appdirglob datadirglob profiles = do
  mappdir  <- wordexp (nosubst <> noundef) appdirglob
  mdatadir <- wordexp (nosubst <> noundef) datadirglob
  case both id (mappdir, mdatadir) of
    Right ([appdir], [datadir]) -> do
      let settings = def & appData .~ datadir & targets .~ targeted profiles
      db <- open settings
      let theses = db^.these.groups
          types  = execWriter (sourceTypes theses) S.empty
          script = execWriter (gen theses appdir types)
      T.putStr script
      hFlush stdout
    _ -> badglob -- wordexp failed
 where
  targeted [] = All
  targeted xs = Children (S.fromList xs)

  sourceTypes db = do
    for_ db $ \(GR groupData) ->
      ifor_ groupData $ \sourceRecord _ ->
        tell (<> S.singleton (sourceType sourceRecord))

  gen db root sources = do
    tell (boilerplate sources)
    ifor_ db $ \groupName (GR groupData) -> do
      write $ group groupName
      case M.null groupData of
        True  -> write $ indent sourceIndent emptyScript
        False ->
          ifor_ groupData $ \sourceRecord fileRecords -> do
            write $ source root sourceRecord
            case S.null fileRecords of
              True  -> write $ indent fileIndent emptyScript
              False ->
                for_ fileRecords $
                  tell . line . file root (sourcePath sourceRecord) (sourceOwner sourceRecord)

  badglob = hPutStrLn stderr $
    "Bad glob pattern: " ++ datadirglob

write :: Monad m => Text -> WriterT Text m ()
write = tell . line


boilerplate :: Foldable t => t String -> Text
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

sourceImports :: Foldable t => t String -> Text
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

group :: String -> Text
group = indent groupIndent . go where
  go name = T.unwords ["group", string (show name), "$ do"]

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

groupIndent, sourceIndent, fileIndent :: Int
groupIndent  = 2
sourceIndent = 4
fileIndent   = 6

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
