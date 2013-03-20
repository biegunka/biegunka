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
action (IS p t _ _ _ u) = nest 3 $ parens (cyan (text u)) </>
  green "update" </> text t </> "source at" </> magenta (text p) </> line
action (IA (Link s d) _ _ n) = nest 3 $ parens (cyan (text n)) </>
  green "link" </> yellow (text d) </> "to" </> magenta (text s) </> line
action (IA (Copy s d) _ _ n) = nest 3 $ parens (cyan (text n)) </>
  green "copy" </> magenta (text s) </> "to" </> yellow (text d) </> line
action (IA (Template s d _) _ _ n) = nest 3 $ parens (cyan (text n)) </>
  green "substitute" </> "templates in" </> magenta (text s) </> "and write to" </> yellow (text d) </> line
action (IA (Shell p c) _ _ n) = nest 3 $ parens (cyan (text n)) </>
  "execute" </> green "shell" </> "`" <//> red (text c) <//> "` from" </> yellow (text p) </> line
action _ = mempty


-- | Describe handled exception
exception :: SomeException -> TermDoc
exception e = nest 3 $ red "FAIL" <//> colon `above` vcat (map text . lines $ show e) </> line


-- | Describe retry counter
retryCounter :: Int -> TermDoc
retryCounter n = yellow "Retry" <//> colon </> text (show n) <//> line
