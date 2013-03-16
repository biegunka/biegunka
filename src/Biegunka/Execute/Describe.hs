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
describe d = "[localhost]" </> d


-- | Describe current action
action :: IL -> TermDoc
action (IS p t _ _ _ u) =
  green "update" </> text t </> "source" </> cyan (text u) </> "at" </> magenta (text p) </> line
action (IA (Link s d) _ _ _) = indent 2 $
  green "link" </> yellow (text d) </> "to" </> magenta (text s) </> line
action (IA (Copy s d) _ _ _) = indent 2 $
  green "copy" </> yellow (text d) </> "to" </> magenta (text s) </> line
action (IA (Template s d _) _ _ _) = indent 2 $
  green "substitute" </> "templates in" </> magenta (text s) </> "and write to" </> yellow (text d) </> line
action (IA (Shell p c) _ _ _) = indent 2 $
  "execute" </> green "shell" </> "`" <//> red (text c) <//> "` from" </> yellow (text p) </> line
action _ = mempty


-- | Describe handled exception
exception :: SomeException -> TermDoc
exception e = red "FAIL" <//> colon `above` text (show e)


-- | Describe retry counter
retryCounter :: Int -> TermDoc
retryCounter n = yellow "Retry" <//> colon </> text (show n) <//> line
