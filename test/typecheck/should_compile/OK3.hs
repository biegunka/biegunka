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
  sudo (username "nobody") $
    git (url "https://example.com/dotfiles.git" . path "/") pass

actions :: Script 'Actions ()
actions =
  sudo (uid 0) $
    link "source" "destination"
