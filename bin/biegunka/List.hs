{-# LANGUAGE LambdaCase #-}
module List where

import           Control.Applicative ((<$>), (<*>))
import           Control.Lens hiding ((<.>))
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Char (toUpper)
import           Data.Default (def)
import           Data.Foldable (for_)
import           Data.List (sort)
import           Data.List.Lens
import           Data.Monoid (Monoid(..), (<>))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import           Data.Traversable (for)
import qualified System.Directory as D
import           System.FilePath ((</>))
import           System.IO (hFlush, hPutStrLn, stderr, stdout)
import           System.FilePath.Lens
import           System.Wordexp (wordexp, nosubst, noundef)

import           Control.Biegunka.Settings (appData)
import           Control.Biegunka.DB (DB(..), GroupRecord(..), SourceRecord(..), FileRecord(..), load)


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
    Right [datadir] -> case profiles of
      []        -> getProfiles (datadir </> "profiles/") >>= mapM_ putStrLn
      profiles' -> case formatText pattern of
        Left errorMessage ->
          badformat errorMessage
        Right formatted -> do
          db <- load (def & appData .~ datadir) profiles'
          T.putStr (execWriter (info formatted db))
          hFlush stdout
 where
  info formatted (DB db) =
    ifor_ db $ \profileName (GR profileData) -> do
      tell $ profileFormat formatted profileName
      ifor_ profileData $ \sourceRecord fileRecords -> do
        tell $ sourceFormat formatted sourceRecord
        for_ fileRecords $ \fileRecord ->
          tell $ fileFormat formatted fileRecord

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

formatText :: String -> Either String (Formatted Text)
formatText = (fmap . fmap) T.pack . format

format :: String -> Either String (Formatted String)
format xs = do
  (x, ys) <- breaking xs
  (y, z)  <- breaking ys
  Formatted <$> formatProfile x <*> formatSource y <*> formatFile z
 where
  formatProfile = formatting $ \case
    'p' -> Right id
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

  formatting :: (Char -> Either String (a -> String)) -> String -> Either String (a -> String)
  formatting rules = \case
    '%':'%':vs -> (\g r -> '%' : g r) <$> formatting rules vs
    '%':'n':vs -> (\g r -> '\n' : g r) <$> formatting rules vs
    '%':vs -> case vs of
      c:cs -> do
        s <- rules c
        t <- formatting rules cs
        return (\a -> s a ++ t a)
      _ -> Left ("incomplete %-placeholder at the end")
    v:vs -> (\g r -> v : g r) <$> formatting rules vs
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
