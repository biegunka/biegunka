module List where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens hiding ((<.>))
import           Data.Char (toUpper)
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.List.Lens
import           Data.Monoid (Monoid(..), (<>))
import           Data.Traversable (for)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)
import           System.FilePath.Lens
import           System.Wordexp (wordexp, nosubst, noundef)

import           Control.Biegunka.Control (appData)
import           Control.Biegunka.DB (DB(..), SourceRecord(..), FileRecord(..), load)


data Formatted a = Formatted
  { profileFormat :: String       -> a
  , sourceFormat  :: SourceRecord -> a
  , fileFormat    :: FileRecord   -> a
  }

instance Functor Formatted where
  fmap g (Formatted p s f) = Formatted (g . p) (g . s) (g . f)


list :: FilePath -> [String] -> String -> IO ()
list datadirglob profiles pattern = do
  mdatadir <- wordexp (nosubst <> noundef) datadirglob
  case mdatadir of
    Left  _         -> badglob -- wordexp failed
    Right (_:_:_)   -> badglob -- multiple matches
    Right []        -> badglob -- wordexp found nothing
    Right [datadir] -> do
      case profiles of
        [] ->
          getProfiles (datadir </> "profiles/") >>= mapM_ putStrLn
        profiles' -> do
          DB db <- load (def & appData .~ datadir) profiles'
          case format pattern of
            Left errorMessage ->
              badformat errorMessage
            Right formatted ->
              ifor_ db $ \profileName profileData -> do
                putStrLn $ profileFormat formatted profileName
                ifor_ profileData $ \sourceRecord fileRecords -> do
                  putStrLn $ sourceFormat formatted sourceRecord
                  for_ fileRecords $ \fileRecord ->
                    putStrLn $ fileFormat formatted fileRecord
 where
  badglob = hPutStrLn stderr $
    "Bad glob pattern: " ++ datadirglob
  badformat message = hPutStrLn stderr $
    "Bad format pattern: " ++ pattern ++ " - " ++ message

getProfiles :: FilePath -> IO [String]
getProfiles root = go root <&> \profiles -> profiles^..folded.prefixed root & sort
 where
  go subroot = do
    isDirectory <- D.doesDirectoryExist subroot
    case isDirectory of
      False -> return $ case subroot^.extension of
        ".profile" -> [subroot&extension.~mempty]
        _          -> []
      True  -> do
        contents <- D.getDirectoryContents subroot <&> filter (`notElem` [".", ".."])
        concat <$> for contents (\path -> go (subroot </> path))

format :: String -> Either String (Formatted String)
format xs =
  let (x, ys) = break (== ';') xs
      (y, zs) = break (== ';') (drop 1 ys)
      z       = drop 1 zs
  in Formatted <$> formatProfile x <*> formatSource y <*> formatFile z
 where
  formatProfile :: String -> Either String (String -> String)
  formatProfile us = case us of
    '%':vs -> case vs of
      'n':ws -> (\f profile -> profile ++ f profile) <$> formatProfile ws
      w:_    -> Left ("%" ++ [w] ++ " is not a placeholder")
      _      -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\f r -> v : f r) <$> formatProfile vs
    []   -> Right (const "")
  formatSource :: String -> Either String (SourceRecord -> String)
  formatSource us = case us of
    '%':vs -> case vs of
      't':ws -> (\f source -> sourceType   source ++ f source) <$> formatSource ws
      'l':ws -> (\f source -> fromLocation source ++ f source) <$> formatSource ws
      'p':ws -> (\f source -> sourcePath   source ++ f source) <$> formatSource ws
      w:_    -> Left ("%" ++ [w] ++ " is not a placeholder")
      _      -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\f r -> v : f r) <$> formatSource vs
    []   -> Right (const "")
  formatFile :: String -> Either String (FileRecord -> String)
  formatFile us = case us of
    '%':vs -> case vs of
      't':ws -> (\f file -> fileType             file  ++ f file) <$> formatFile ws
      'T':ws -> (\f file -> capitalize (fileType file) ++ f file) <$> formatFile ws
      'l':ws -> (\f file -> fromSource           file  ++ f file) <$> formatFile ws
      'p':ws -> (\f file -> filePath             file  ++ f file) <$> formatFile ws
      w:_    -> Left ("%" ++ [w] ++ " is not a placeholder")
      _      -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\f r -> v : f r) <$> formatFile vs
    []   -> Right (const "")

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""
