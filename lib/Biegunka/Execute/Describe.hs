{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Describe execution I/O actions
module Biegunka.Execute.Describe
  ( -- * General description formatting
    describe
    -- * Specific description formatting
  , action, exception, retryCounter
  ) where

import Control.Exception (SomeException)
import Data.List (intercalate)
import Data.Monoid (mempty)

import System.Process (CmdSpec(..))
import Text.PrettyPrint.ANSI.Leijen

import Biegunka.Language
import Biegunka.Script


-- | Describe current action and host where it happens
describe :: Doc -> Doc
describe d = let host = "[localhost]" :: String in nest (length host) (text host </> d) <> linebreak


-- | Describe current action
action :: EL SA s a -> Doc
action il = nest 3 $ case il of
  ES _ (S t u d _) _ _  -> annotation (text u) $
    green "update" </> text t </> "source at" </> magenta (text d)
  EA (SAA { saaURI, saaOrder, saaMaxOrder } ) a _ ->
    annotation (text saaURI) $ progress saaOrder saaMaxOrder <$> case a of
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
