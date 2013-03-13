{-# LANGUAGE OverloadedStrings #-}
module Biegunka.Log (full) where

import Data.List ((\\))
import Data.Maybe (mapMaybe)
import System.Console.Terminfo.PrettyPrint
import Text.PrettyPrint.Free

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.Language (IL(..), A(..), W(..))


full :: [IL] -> Biegunka -> Biegunka -> TermDoc
full cs s t = vcat (mapMaybe install cs ++ [empty] ++ uninstall s t ++ [empty])


install :: IL -> Maybe TermDoc
install (IS p t _ _ _ u) =
  Just $ text t </> "source" </> cyan (text u) </> "at" </> magenta (text p)
install (IA (Link src dst) _ _ _) = Just . indent 2 $
  yellow (text dst) </> green "links" </> "to" </> magenta (text src)
install (IA (Copy src dst) _ _ _) = Just . indent 2 $
  yellow (text dst) </> "is a" </> green "copy" </> "of" </> magenta (text src)
install (IA (Template src dst _) _ _ _) = Just . indent 2 $
  yellow (text dst) </> "is copied with substituted" </> green "templates" </> "from" </> magenta (text src)
install (IA (Shell p c) _ _ _) = Just . indent 2 $
  green "shell" </> "`" <//> red (text c) <//> "` executed from" </> yellow (text p)
install (IW a)           = go a
 where
  go (User (Just user)) = Just $ green "change user" </> "to" </> text user
  go (User Nothing)     = Just $ green "change user" </> "back"
  go _                  = Nothing


uninstall :: Biegunka -> Biegunka -> [TermDoc]
uninstall a b = map ("Delete" </>) $
  map text (filepaths a \\ filepaths b) ++ map text (sources a \\ sources b)
