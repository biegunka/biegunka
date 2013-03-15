{-# LANGUAGE OverloadedStrings #-}
module Biegunka.Execute.Narrator
  ( describe
  ) where

import Data.Monoid (mempty)

import Text.PrettyPrint.Free
import System.Console.Terminfo.PrettyPrint

import Biegunka.Language


-- | Describe current action and host where it happens
describe :: IL -> TermDoc
describe a = "[localhost]" </> action a


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
