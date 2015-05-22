{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( -- * General description formatting
    runChanges
    -- * Specific description formatting
  , action, exception, removal
  ) where

import Control.Exception (SomeException)
import Data.List ((\\), intercalate)
import Text.Printf (printf)

import Control.Lens
import System.Process (CmdSpec(..))

import Control.Biegunka.Namespace (Partitioned, Namespaces, these, files, sources)
import Control.Biegunka.Language
import Control.Biegunka.Script

-- | Describe an action
action :: Retries -> Maybe String -> Term Annotate s a -> String
action (Retries n) mout ta =
  case mout of
    Nothing -> unlines
      [ prefixf ta
      , "  * " ++ doc
               ++ " (up-to-date)"
               ++ if n > 0 then printf " [%sretry %d%s]" yellow n reset else ""
      ]
    Just out -> unlines
      [ prefixf ta
      , "  * " ++ doc
               ++ if n > 0 then printf " [%sretry %d%s]" yellow n reset else ""
               ++ printf "\n    %s- %s%s" green out reset
      ]
 where
  prefixf :: Term Annotate s a -> String
  prefixf (TS (AS { asSegments }) (Source _ url _ _) _ _) =
    intercalate "::" (reverse (printf "[%s]" url : asSegments))
  prefixf (TA (AA { aaSegments, aaURI }) _ _) =
    intercalate "::" (reverse (printf "[%s]" aaURI : aaSegments))
  prefixf (TM _ _) = ""

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

-- | Describe handled exception
exception :: SomeException -> String
exception e =
  unlines ( printf "[%sexception%s]:" red reset
          : map ("  " ++) (lines (show e)))

-- | Describe file or directory removal
removal :: FilePath -> String
removal = printf "Removing: %s\n"

-- | Describe changes which will happen after the run
runChanges :: Partitioned Namespaces -> Namespaces -> String
runChanges db gs = unlines $ "" : concatMap about
  [ ("added files",     green, (files gs \\ files (view these db)))
  , ("added sources",   green, (sources gs \\ sources (view these db)))
  , ("deleted files",   red,   (files (view these db) \\ files gs))
  , ("deleted sources", red,   (sources (view these db) \\ sources gs))
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
