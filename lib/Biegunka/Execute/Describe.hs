{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Biegunka.Execute.Describe
  ( -- * General description formatting
    describe, stats
    -- * Specific description formatting
  , action, exception, retryCounter
  ) where

import Control.Exception (SomeException)
import Data.List ((\\), intercalate)
import Data.Maybe (mapMaybe)
import Data.Monoid (mempty)

import System.Process (CmdSpec(..))
import Text.PrettyPrint.ANSI.Leijen

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.Language
import Biegunka.Script


-- | Describe current action and host where it happens
describe :: Doc -> Doc
describe d = let host = "[localhost]" :: String in nest (length host) (text host </> d) <> linebreak


-- | Describe current action
action :: Term Annotate s a -> Doc
action il = nest 3 $ case il of
  ES _ (S t u d _) _ _  -> annotation (text u) $
    green "update" </> text t </> "source at" </> magenta (text d)
  EA (AA { aaURI, aaOrder, aaMaxOrder } ) a _ ->
    annotation (text aaURI) $ progress aaOrder aaMaxOrder <$> case a of
      Link s d       -> green "link" </> yellow (text d) </> "to" </> magenta (text s)
      Copy s d       -> green "copy" </> magenta (text s) </> "to" </> yellow (text d)
      Template s d _ -> green "substitute" </> "in" </> magenta (text s) </> "to" </> yellow (text d)
      Shell p (ShellCommand c) ->
        green "shell command" </> "`" <//> red (text c) <//> "' from" </> yellow (text p)
      Shell p (RawCommand c as) ->
        green "command" </> "`" <//> red (text (intercalate " " (c:as))) <//> "' from" </> yellow (text p)
  _ -> mempty
 where
  -- | Annotate action description with source name
  annotation :: Doc -> Doc -> Doc
  annotation t doc = parens (cyan t) </> doc

  -- | Add progress to action description
  progress :: Int -> Int -> Doc
  progress n mn = brackets (pretty n <> "/" <> pretty mn)


-- | Describe handled exception
exception :: SomeException -> Doc
exception e = nest 3 $ (red "FAIL" <//> colon) <$> vcat (map text . lines $ show e)


-- | Describe retry counter
retryCounter :: Int -> Doc
retryCounter n = yellow "Retry" <//> colon </> text (show n)


stats :: Biegunka -> Biegunka -> Doc
stats a b = vcat $ empty : mapMaybe about
  [ ("added files",     map (yellow  . text) $ filepaths b \\ filepaths a)
  , ("added sources",   map (magenta . text) $ sources b   \\ sources a)
  , ("deleted files",   map (yellow  . text) $ filepaths a \\ filepaths b)
  , ("deleted sources", map (magenta . text) $ sources a   \\ sources b)
  ] ++ [empty]
 where
  about (msg, xs) = case length xs of
    0 -> Nothing
    n -> Just $ nest 2 ((msg </> parens (pretty n) <//> colon) <$> vcat (xs ++ [empty]))
