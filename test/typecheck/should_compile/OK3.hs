{-# LANGUAGE DataKinds #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = pass

sources :: Script 'Sources ()
sources =
  sudo $
    git (origin "https://example.com/dotfiles.git" . path "/") pass

actions :: Script 'Actions ()
actions =
  sudo $
    link (origin "source" . path "destination")
