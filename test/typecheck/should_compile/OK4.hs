{-# LANGUAGE DataKinds #-}
-- |
-- Checks that reacting works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = pass

sources :: Script 'Sources ()
sources =
  reacting Ignorant $
    git (url "https://example.com/dotfiles.git" . path "/") pass

actions :: Script 'Actions ()
actions =
  reacting Abortive $
    link "source" "destination"
