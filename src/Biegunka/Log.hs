{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Log (full) where

import Data.List ((\\))
import Data.Maybe (mapMaybe)

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.Language (Command(..), Action(..), Wrapper(..))


full :: [Command l () b] -> Biegunka -> Biegunka -> String
full cs s t = unlines $ mapMaybe install cs ++ uninstall s t


install :: Command l () b -> Maybe String
install (P name _ _)    = Just $ "Setup profile [" ++ name ++ "]"
install (S t u p _ _ _) = Just $ indent 2 ++ "Setup " ++ t ++ " repository " ++ u ++ " at " ++ p
install (F a _)         = Just $ indent 4 ++ go a
 where
  go (Link src dst)       = "Link file " ++ src ++ " to " ++ dst
  go (Copy src dst)       = "Copy file " ++ src ++ " to " ++ dst
  go (Template src dst _) = "Write " ++ src ++ " with substituted templates to " ++ dst
  go (Shell p c)          = "Shell command `" ++ c ++ "` from " ++ p
install (W a _)         = go a
 where
  go (User (Just user)) = Just $ "--- * Do stuff from user " ++ user ++ " * ---"
  go (User Nothing)     = Just $ "--- * Do stuff from default user * ---"
  go _                  = Nothing

indent :: Int -> String
indent n = replicate n ' '


uninstall :: Biegunka -> Biegunka -> [String]
uninstall a b = map ("Delete " ++) $
  (filepaths a \\ filepaths b) ++ (sources a \\ sources b)
