{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Dry run interpreter
module Biegunka.Pretend (dryRun, pretend) where

import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Prelude hiding (log)

import Control.Lens
import Control.Monad.Free (Free(..))
import Text.PrettyPrint.ANSI.Leijen

import Biegunka.DB
import Biegunka.Language (EL(..), P(..), S(..), A(..), M(..))
import Biegunka.Control (Interpreter(..), interpret, logger)
import Biegunka.Script (SA)


-- | Dru run interpreter
dryRun :: Interpreter
dryRun = interpret $ \c s -> do
  let b = construct s
  a <- load c s
  view logger c (log s a b)
  view logger c (stats a b)

-- | Dru run interpreter
pretend :: Interpreter
pretend = dryRun
{-# DEPRECATED pretend "Please, use `dryRun'" #-}

stats :: Biegunka -> Biegunka -> Doc
stats a b = vcat $ mapMaybe about
  [ ("added files",     map (yellow  . text) $ filepaths b \\ filepaths a)
  , ("added sources",   map (magenta . text) $ sources b   \\ sources a)
  , ("deleted files",   map (yellow  . text) $ filepaths a \\ filepaths b)
  , ("deleted sources", map (magenta . text) $ sources a   \\ sources b)
  ] ++ [empty]
 where
  about (msg, xs) = case length xs of
    0 -> Nothing
    n -> Just $ nest 2 ((msg </> parens (pretty n) <//> colon) <$> vcat (xs ++ [empty]))


log :: Free (EL SA s) a -> Biegunka -> Biegunka -> Doc
log cs a b = vcat (install cs ++ [empty] ++ uninstall ++ [empty])
 where
  install :: Free (EL SA s) a -> [Doc]
  install (Free (EP _ (P n) i z)) =
      (green "profile" </> cyan (text n) <> ":")
    : map (indent 2) (install i) ++ install z
  install (Free (ES _ (S t u d _) i z)) =
      (green "update" </> text t </> "source" </> cyan (text u) </> "at" </> magenta (text d))
    : map (indent 2) (install i) ++ install z
  install (Free (EA _ i z)) = (:install z) $ case i of
    Link s d ->
      yellow (text d) </> green "links" </> "to" </> magenta (text s)
    Copy s d ->
      yellow (text d) </> "is a" </> green "copy" </> "of" </> magenta (text s)
    Template s d _ ->
      yellow (text d) </> "is copied with substituted" </> green "templates" </> "from" </> magenta (text s)
    Shell p c ->
      green "shell" </> "`" <//> red (text c) <//> "` executed from" </> yellow (text p)
  install (Free (EM w z)) = go w
   where
    go (User (Just user)) = (green "change user" </> "to" </> text user) : install z
    go (User Nothing)     = (green "change user" </> "back") : install z
    go _                  = install z
  install (Pure _) = []

  uninstall :: [Doc]
  uninstall = map ("Delete" </>) $
    map (yellow . text) (filepaths a \\ filepaths b) ++ map (magenta . text) (sources a \\ sources b)
