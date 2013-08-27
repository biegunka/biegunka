{-# LANGUAGE LambdaCase #-}
module List where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens hiding ((<.>))
import           Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char (toUpper)
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.List.Lens
import           Data.Monoid (Monoid(..), (<>))
import qualified Data.Set as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Traversable (for)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO (hFlush, hPutStrLn, stderr, stdout)
import           System.FilePath.Lens
import           System.Wordexp (wordexp, nosubst, noundef)

import           Control.Biegunka.Settings
  (appData, Targets(..), targets)
import           Control.Biegunka.Groups
  (GroupRecord(..), SourceRecord(..), FileRecord(..), these, groups, open, who)

import Options


data Formatted a = Formatted
  { profileFormat :: String       -> a
  , sourceFormat  :: SourceRecord -> a
  , fileFormat    :: FileRecord   -> a
  }

instance Functor Formatted where
  fmap g (Formatted p s f) = Formatted (g . p) (g . s) (g . f)


list :: FilePath -> [String] -> Format -> IO ()
list datadirglob profiles format = do
  mdatadir <- wordexp (nosubst <> noundef) datadirglob
  case mdatadir of
    Left  _         -> badglob -- wordexp failed
    Right (_:_:_)   -> badglob -- multiple matches
    Right []        -> badglob -- wordexp found nothing
    Right [datadir] ->
      let settings = def & appData .~ datadir & targets .~ targeted profiles
      in case format of
        Format pattern -> case formattingText pattern of
          Left errorMessage ->
            badformat errorMessage pattern
          Right formatted -> do
            db <- open settings
            T.putStr (execWriter (info formatted (db^.these.groups)))
            hFlush stdout
        JSON -> do
          db <- open settings
          B.putStrLn . A.encode $ db^.these
 where
  targeted [] = All
  targeted xs = Children (S.fromList xs)


  info formatted db =
    ifor_ db $ \profileName (GR profileData) -> do
      tell $ profileFormat formatted profileName
      ifor_ profileData $ \sourceRecord fileRecords -> do
        tell $ sourceFormat formatted sourceRecord
        for_ fileRecords $ \fileRecord ->
          tell $ fileFormat formatted fileRecord

  badglob = hPutStrLn stderr $
    "Bad glob pattern: " ++ datadirglob
  badformat message pattern = hPutStrLn stderr $
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

formattingText :: String -> Either String (Formatted Text)
formattingText = (fmap . fmap) T.pack . formatting

formatting :: String -> Either String (Formatted String)
formatting xs = do
  (x, ys) <- breaking xs
  (y, z)  <- breaking ys
  Formatted <$> formatProfile x <*> formatSource y <*> formatFile z
 where
  formatProfile = format $ \case
    'p' -> Right id
    c   -> Left ("%" ++ [c] ++ " is not a group info placeholder")

  formatSource = format $ \case
    't' -> Right sourceType
    'l' -> Right fromLocation
    'p' -> Right sourcePath
    'u' -> Right (who . sourceOwner)
    c   -> Left ("%" ++ [c] ++ " is not a source info placeholder")

  formatFile = format $ \case
    't' -> Right fileType
    'T' -> Right (capitalize . fileType)
    'l' -> Right fromSource
    'p' -> Right filePath
    'u' -> Right (who . fileOwner)
    c   -> Left ("%" ++ [c] ++ " is not a file info placeholder")

  format :: (Char -> Either String (a -> String)) -> String -> Either String (a -> String)
  format rules = \case
    '%':'%':vs -> (\g r -> '%' : g r) <$> format rules vs
    '%':'n':vs -> (\g r -> '\n' : g r) <$> format rules vs
    '%':vs -> case vs of
      c:cs -> do
        s <- rules c
        t <- format rules cs
        return (\a -> s a ++ t a)
      _ -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\g r -> v : g r) <$> format rules vs
    []   -> Right (const "")

-- | Break string on "%;"
--
-- >>> breaking "hello%;world"
-- Right ("hello","world")
--
-- >>> breaking "hello%;"
-- Right ("hello","")
--
-- >>> breaking "%;world"
-- Right ("","world")
--
-- >>> breaking "%;"
-- Right ("","")
--
-- >>> breaking "he%nllo%;wo%mrld"
-- Right ("he%nllo","wo%mrld")
--
-- >>> breaking "%"
-- Left "Formatting section is missing"
--
-- >>> breaking "123hello"
-- Left "Formatting section is missing"
breaking :: String -> Either String (String, String)
breaking xs = case break (== '%') xs of
  (ys, _:';':zs) -> Right (ys, zs)
  (ys, _:c:zs)   -> (\(a, b) -> (ys ++ ['%',c] ++ a, b)) <$> breaking zs
  (_, _)         -> Left "Formatting section is missing"

-- | Make word's first letter uppercase
--
-- >>> capitalize "hello"
-- "Hello"
--
-- >>> capitalize "Hello"
-- "Hello"
--
-- >>> capitalize "123hello"
-- "123hello"
capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize ""     = ""
