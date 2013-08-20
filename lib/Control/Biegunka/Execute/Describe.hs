{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Control.Biegunka.Execute.Describe
  ( -- * General description formatting
    termDescription, runChanges
    -- * Specific description formatting
  , action, exception, retryCounter
  ) where

import Control.Exception (SomeException)
import Data.List ((\\), intercalate)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)

import Control.Lens
import System.Process (CmdSpec(..))
import Text.PrettyPrint.ANSI.Leijen

import Control.Biegunka.Settings
  ( ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  )
import Control.Biegunka.DB (DB, Groups, here, files, sources)
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
        (sc^.actionColor) "update"
    </> text t
    </> "source at"
    </> (sc^.dstColor) (text d)
  TA (AA { aaURI, aaOrder, aaMaxOrder } ) a _ ->
    annotation (text aaURI) $ progress aaOrder aaMaxOrder <$> case a of
      Link s d       ->
            (sc^.actionColor) "link"
        </> (sc^.srcColor) (text d)
        </> "to"
        </> (sc^.dstColor) (text s)
      Copy s d _     ->
            (sc^.actionColor) "copy"
        </> (sc^.srcColor) (text s)
        </> "to"
        </> (sc^.dstColor) (text d)
      Template s d _ ->
            (sc^.actionColor) "substitute"
        </> "in"
        </> (sc^.srcColor) (text s)
        </> "to"
        </> (sc^.dstColor) (text d)
      Command p (ShellCommand c) ->
            (sc^.actionColor) "shell command"
        </> "`"
        <//> text c
        <//> "' from"
        </> (sc^.srcColor) (text p)
      Command p (RawCommand c as) ->
            (sc^.actionColor) "external command"
        </> "`"
        <//> text (intercalate " " (c:as))
        <//> "' from"
        </> (sc^.srcColor) (text p)
      Patch patch root PatchSpec { reversely } ->
            (sc^.actionColor) "patch"
        </> (sc^.srcColor) (text patch)
        </> (if reversely then parens "reversely" </> "applied" else "applied")
        </> "to"
        </> (sc^.dstColor) (text root)
  _ -> mempty
 where
  -- | Annotate action description with source name
  annotation :: Doc -> Doc -> Doc
  annotation t doc = parens ((sc^.sourceColor) t) </> doc

  -- | Add progress to action description
  progress :: Int -> Int -> Doc
  progress n mn = brackets (pretty n <> "/" <> pretty mn)


-- | Describe handled exception
exception :: ColorScheme -> SomeException -> Doc
exception sc e = nest 3 $
  ((sc^.errorColor) "ERROR" <//> colon) <$> vcat (map text . lines $ show e)


-- | Describe retry counter
retryCounter :: ColorScheme -> Int -> Int -> Doc
retryCounter sc m n =
      (sc^.retryColor) "Retry"
  </> text (show m)
  </> (sc^.retryColor) "out of"
  </> text (show n)
  <//> (sc^.retryColor) colon


-- | Describe changes which will happen after the run
runChanges :: ColorScheme -> DB -> Groups -> Doc
runChanges sc db gs = vcat $ empty : mapMaybe about
  [ ("added files",     map ((sc^.srcColor) . text) $ files gs \\ files (db^.here))
  , ("added sources",   map ((sc^.dstColor) . text) $ sources gs   \\ sources (db^.here))
  , ("deleted files",   map ((sc^.srcColor) . text) $ files (db^.here) \\ files gs)
  , ("deleted sources", map ((sc^.dstColor) . text) $ sources (db^.here)   \\ sources gs)
  ] ++ [empty]
 where
  about (msg, xs) = case length xs of
    0 -> Nothing
    n -> Just $ nest 2 ((msg </> parens (pretty n) <//> colon) <$> vcat (xs ++ [empty]))
