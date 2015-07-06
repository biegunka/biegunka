{-# LANGUAGE DataKinds #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = return ()

sources :: Script 'Sources ()
sources =
  sudo (username "nobody") $
    git_ (url "https://example.com/dotfiles.git" . path "/")

actions :: Script 'Actions ()
actions =
  sudo (uid 0) $
    link "source" "destination"
