{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( -- * General description formatting
    termDescription, runChanges
    -- * Specific description formatting
  , action, exception, retryCounter, removal
  ) where

import Control.Exception (SomeException)
import Data.List ((\\))
import Data.Maybe (mapMaybe)

import Control.Lens
import System.Process (CmdSpec(..))
import Text.PrettyPrint.ANSI.Leijen

import Control.Biegunka.Settings
  ( ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  )
import Control.Biegunka.Groups (Partitioned, Groups, these, files, sources)
import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Describe current action and host where it happens
termDescription :: Doc -> Doc
termDescription d =
  let host = "[localhost]" :: String
  in nest (length host) (text host </> d) <> linebreak


-- | Describe current action
action :: ColorScheme -> Term Annotate s a -> Doc
action sc il = nest 3 $ case il of
  TS _ (Source t u d _) _ _  -> annotation (text u) $
        view actionColor sc "update"
    </> text t
    </> "source at"
    </> view dstColor sc (text d)
  TA (AA { aaURI, aaOrder, aaMaxOrder } ) a _ ->
    annotation (text aaURI) $ progress aaOrder aaMaxOrder <> line <> case a of
      Link s d       ->
            view actionColor sc "link"
        </> view srcColor sc (text d)
        </> "to"
        </> view dstColor sc (text s)
      Copy s d _     ->
            view actionColor sc "copy"
        </> view srcColor sc (text s)
        </> "to"
        </> view dstColor sc (text d)
      Template s d _ ->
            view actionColor sc "substitute"
        </> "in"
        </> view srcColor sc (text s)
        </> "to"
        </> view dstColor sc (text d)
      Command p (ShellCommand c) ->
            view actionColor sc "shell command"
        </> "`"
        <//> text c
        <//> "' from"
        </> view srcColor sc (text p)
      Command p (RawCommand c as) ->
            view actionColor sc "external command"
        </> "`"
        <//> text (unwords (c:as))
        <//> "' from"
        </> view srcColor sc (text p)
      Patch patch file PatchSpec { reversely } ->
            view actionColor sc "patch"
        </> view srcColor sc (text patch)
        </> (if reversely then parens "reversely" </> "applied" else "applied")
        </> "to"
        </> view dstColor sc (text file)
  _ -> empty
 where
  -- | Annotate action description with source name
  annotation :: Doc -> Doc -> Doc
  annotation t doc = parens (view sourceColor sc t) </> doc

  -- | Add progress to action description
  progress :: Int -> Int -> Doc
  progress n mn = brackets (pretty n <> "/" <> pretty mn)


-- | Describe handled exception
exception :: ColorScheme -> SomeException -> Doc
exception sc e = nest 3 $
  (view errorColor sc "ERROR" <//> colon) <> line <>  vcat (map text . lines $ show e)


-- | Describe retry counter
retryCounter :: ColorScheme -> Int -> Int -> Doc
retryCounter sc m n =
      view retryColor sc "Retry"
  </> text (show m)
  </> view retryColor sc "out of"
  </> text (show n)
  <//> view retryColor sc colon


-- | Describe file or directory removal
removal :: FilePath -> Doc
removal path = "Removing" <> colon </> text path <> line


-- | Describe changes which will happen after the run
runChanges :: ColorScheme -> Partitioned Groups -> Groups -> Doc
runChanges sc db gs = vcat $ empty : mapMaybe about
  [ ("added files",     map (view srcColor sc . text) (files gs \\ files (view these db)))
  , ("added sources",   map (view dstColor sc . text) (sources gs \\ sources (view these db)))
  , ("deleted files",   map (view srcColor sc . text) (files (view these db) \\ files gs))
  , ("deleted sources", map (view dstColor sc . text) (sources (view these db) \\ sources gs))
  ] ++ [empty]
 where
  about (msg, xs) = case length xs of
    0 -> Nothing
    n -> Just $ nest 2 ((msg </> parens (pretty n) <//> colon) <> line <> vcat (xs ++ [empty]))
