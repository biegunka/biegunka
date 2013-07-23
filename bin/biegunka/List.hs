module List where

import           Control.Applicative ((<$>))
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
          let formatted = format pattern
          ifor_ db $ \profileName profileData -> do
            putStrLn $ profileFormat formatted profileName
            ifor_ profileData $ \sourceRecord fileRecords -> do
              putStrLn $ sourceFormat formatted sourceRecord
              for_ fileRecords $ \fileRecord ->
                putStrLn $ fileFormat formatted fileRecord
 where
  badglob = putStrLn $ "Bad glob pattern: " ++ datadirglob

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

format :: String -> Formatted String
format xs =
  let (x, ys) = break (== ';') xs
      (y, zs) = break (== ';') (drop 1 ys)
      z       = drop 1 zs
  in Formatted (formatProfile x) (formatSource y) (formatFile z)
 where
  formatProfile us profile = case us of
    '%':vs -> case vs of
      'n':ws -> profile ++ formatProfile ws profile
      _ -> error "P"
    v:vs -> v : formatProfile vs profile
    []   -> ""
  formatSource us source = case us of
    '%':vs -> case vs of
      't':ws -> sourceType   source ++ formatSource ws source
      'l':ws -> fromLocation source ++ formatSource ws source
      'p':ws -> sourcePath   source ++ formatSource ws source
      _ -> error "S"
    v:vs -> v : formatSource vs source
    []   -> ""
  formatFile us file = case us of
    '%':vs -> case vs of
      't':ws -> fileType             file  ++ formatFile ws file
      'T':ws -> capitalize (fileType file) ++ formatFile ws file
      'l':ws -> fromSource           file  ++ formatFile ws file
      'p':ws -> filePath             file  ++ formatFile ws file
      '%':ws -> '%' : formatFile ws file
      _ -> error "F"
    v:vs -> v : formatFile vs file
    []   -> ""

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""
