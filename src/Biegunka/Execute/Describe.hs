{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Biegunka.Execute.Describe
  ( -- * General description formatting
    describe
    -- * Specific description formatting
  , action, exception, retryCounter
  ) where

import Control.Exception (SomeException)
import Data.Monoid (mempty)

import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

import Biegunka.Language


-- | Describe current action and host where it happens
describe :: TermDoc -> TermDoc
describe d = let host = "[localhost]" :: String in nest (length host) $ text host </> d


-- | Describe current action
action :: IL -> TermDoc
action il = nest 3 $ case il of
  IS p t _ _ u  -> annotation (text u) $
    green "update" </> text t </> "source at" </> magenta (text p) </> line
  IA a o om _ n -> annotation (text n) $ progress o om </> case a of
    Link s d       -> green "link" </> yellow (text d) </> "to" </> magenta (text s) </> line
    Copy s d       -> green "copy" </> magenta (text s) </> "to" </> yellow (text d) </> line
    Template s d _ -> green "substitute" </> "in" </> magenta (text s) </> "to" </> yellow (text d) </> line
    Shell p c      -> green "shell" </> "`" <//> red (text c) <//> "` from" </> yellow (text p) </> line
  _ -> mempty
 where
  -- | Annotate action description with source name
  annotation :: TermDoc -> TermDoc -> TermDoc
  annotation t doc = parens (cyan t) </> doc

  -- | Add progress to action description
  progress :: Int -> Int -> TermDoc
  progress n mn = brackets (pretty n <> "/" <> pretty mn)


-- | Describe handled exception
exception :: SomeException -> TermDoc
exception e = nest 3 $ red "FAIL" <//> colon `above` vcat (map text . lines $ show e) </> line


-- | Describe retry counter
retryCounter :: Int -> TermDoc
retryCounter n = yellow "Retry" <//> colon </> text (show n) <//> line
