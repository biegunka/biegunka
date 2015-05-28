{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( -- * General description formatting
    runChanges
    -- * Specific description formatting
  , describeTerm, removal
  , sourceIdentifier
  ) where

import           Control.Exception (SomeException)
import           Data.Bool (bool)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Text.Printf (printf)

import           Control.Lens
import           System.Process (CmdSpec(..))

import           Control.Biegunka.Namespace (Partitioned, Namespaces, these, files, sources)
import           Control.Biegunka.Language
import           Control.Biegunka.Script

-- | Describe an action and its outcome.
describeTerm
  :: Retries
  -> Either SomeException (Maybe String)
  -> Bool
  -> Term Annotate s a
  -> String
describeTerm (Retries n) mout withSource ta =
  case mout of
    Left e -> unlines .
      (if withSource then (prefixf ta :) else id) $
        ("  * " ++ doc
                ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
                ++ printf " [%sexception%s]" red reset)
        : map (\l -> printf "    %s%s%s" red l reset) (lines (show e))
    Right Nothing -> unlines $
      (if withSource then (prefixf ta :) else id)
        [ "  * " ++ doc
                 ++ " (up-to-date)"
                 ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
        ]
    Right (Just out) -> unlines $
      (if withSource then (prefixf ta :) else id)
        [ "  * " ++ doc
                 ++ bool "" (printf " [%sretry %d%s]" yellow n reset) (n > 0)
                 ++ printf "\n    %s- %s%s" green out reset
        ]
 where
  prefixf :: Term Annotate s a -> String
  prefixf t = case sourceIdentifier t of
    Nothing -> ""
    Just (url :| ns) ->
      List.intercalate "::" (reverse (printf "[%s]" url : ns))

  doc = case ta of
    TS _ (Source t _ d _) _ _  ->
      printf "%s source[%s] update" t d
    TA _ a _ ->
      case a of
        Link s d       ->
          printf "symlink[%s] update (point to [%s])" d s
        Copy s d _     ->
          printf "file[%s] update (copy [%s])" d s
        Template s d _ ->
          printf "file[%s] update (interpolate [%s])" d s
        Command p (ShellCommand c) ->
          printf "execute[%s] (from [%s])" c p
        Command p (RawCommand c as) ->
          printf "execute[%s] (from [%s])" (unwords (c : as)) p
    _ -> ""

-- | Note that the components are in the reverse order.
sourceIdentifier :: Term Annotate s a -> Maybe (NonEmpty String)
sourceIdentifier = \case
  TS (AS { asSegments }) (Source _ url _ _) _ _ -> Just (url :| asSegments)
  TA (AA { aaSegments, aaURI }) _ _ -> Just (aaURI :| aaSegments)
  TM _ _ -> Nothing

-- | Describe file or directory removal
removal :: FilePath -> String
removal = printf "Removing: %s\n"

-- | Describe changes which will happen after the run
runChanges :: Partitioned Namespaces -> Namespaces -> String
runChanges db gs = unlines $ "" : concatMap about
  [ ("added files",     green, (files gs List.\\ files (view these db)))
  , ("added sources",   green, (sources gs List.\\ sources (view these db)))
  , ("deleted files",   red,   (files (view these db) List.\\ files gs))
  , ("deleted sources", red,   (sources (view these db) List.\\ sources gs))
  ] ++ [""]
 where
  about (msg, color, xs) = case length xs of
    0 -> []
    n -> printf "%s (%d):" msg n : map (\x -> "  " ++ color ++ x ++ reset) xs

red :: String
red = "\ESC[31;2m"

yellow :: String
yellow = "\ESC[33;2m"

green :: String
green = "\ESC[32;2m"

reset :: String
reset = "\ESC[0m"
