{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Biegunka.Execute.Describe
  ( -- * General description formatting
    describe
    -- * Specific description formatting
  , action, exception, retryCounter
  ) where

import Control.Exception (SomeException)

import Text.PrettyPrint.ANSI.Leijen

import Biegunka.Language
import Biegunka.Script


-- | Describe current action and host where it happens
describe :: Doc -> Doc
describe d = let host = "[localhost]" :: String in nest (length host) (text host </> d) <> linebreak


-- | Describe current action
action :: EL (SA s) s a -> Maybe Doc
action il = nest 3 `fmap` case il of
  ES _ (Source t u d _) _ _  -> Just . annotation (text u) $
    green "update" </> text t </> "source at" </> magenta (text d)
  EA _ a _ -> Just . annotation (text "M") $ progress 4 7 <$> case a of
    Link s d       -> green "link" </> yellow (text d) </> "to" </> magenta (text s)
    Copy s d       -> green "copy" </> magenta (text s) </> "to" </> yellow (text d)
    Template s d _ -> green "substitute" </> "in" </> magenta (text s) </> "to" </> yellow (text d)
    Shell p c      -> green "shell" </> "`" <//> red (text c) <//> "` from" </> yellow (text p)
  _ -> Nothing
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
