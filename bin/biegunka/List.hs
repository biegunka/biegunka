{-# LANGUAGE LambdaCase #-}
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
  formatProfile = formatting "n" $ \profile -> \case
    'n' -> profile
    _   -> error "Impossible"

  formatSource = formatting "tlp" $ \source -> \case
    't' -> sourceType source
    'l' -> fromLocation source
    'p' -> sourcePath source
    _   -> error "Impossible"

  formatFile = formatting "tTlp" $ \file -> \case
    't' -> fileType file
    'T' -> capitalize (fileType file)
    'l' -> fromSource file
    'p' -> filePath file
    _   -> error "Impossible"

  formatting :: String -> (a -> Char -> String) -> String -> Either String (a -> String)
  formatting us h = \case
    '%':vs -> case vs of
      '%':ws -> (\g r -> '%' : g r) <$> formatting us h ws
      w:ws
        | w `elem` us -> (\g r -> h r w ++ g r) <$> formatting us h ws
        | otherwise   -> Left ("%" ++ [w] ++ " is not a placeholder")
      _ -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\g r -> v : g r) <$> formatting us h vs
    []   -> Right (const "")

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""
