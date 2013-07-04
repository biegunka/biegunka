{-# LANGUAGE DataKinds #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = return ()

profiles :: Script Profiles ()
profiles =
  sudo "nobody" $
    profile "name" $
      return ()

sources :: Script Sources ()
sources =
  sudo "nobody" $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  sudo "nobody" $
    link "source" "destination"
