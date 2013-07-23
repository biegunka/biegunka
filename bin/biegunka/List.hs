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
    "Bad format pattern: \"" ++ pattern ++ "\" - " ++ message

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
format xs = do
  (x, ys) <- breaking xs
  (y, z)  <- breaking ys
  Formatted <$> formatProfile x <*> formatSource y <*> formatFile z
 where
  formatProfile = formatting $ \case
    'n' -> Right id
    c   -> Left ("%" ++ [c] ++ " is not a profile info placeholder")

  formatSource = formatting $ \case
    't' -> Right sourceType
    'l' -> Right fromLocation
    'p' -> Right sourcePath
    c   -> Left ("%" ++ [c] ++ " is not a source info placeholder")

  formatFile = formatting $ \case
    't' -> Right fileType
    'T' -> Right (capitalize . fileType)
    'l' -> Right fromSource
    'p' -> Right filePath
    c   -> Left ("%" ++ [c] ++ " is not a file info placeholder")

  breaking us = case break (== ';') us of
    (_, [])   -> Left "Section missing"
    (v, _:ws) -> Right (v, ws)

  formatting :: (Char -> Either String (a -> String)) -> String -> Either String (a -> String)
  formatting rules = \case
    '%':'%':vs -> (\g r -> '%' : g r) <$> formatting rules vs
    '%':vs -> case vs of
      c:cs -> do
        s <- rules c
        t <- formatting rules cs
        return (\a -> s a ++ t a)
      _ -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\g r -> v : g r) <$> formatting rules vs
    []   -> Right (const "")

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""