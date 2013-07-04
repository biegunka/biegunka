{-# LANGUAGE DataKinds #-}
module Main where


import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = return ()

profiles :: Script Profiles ()
profiles =
  reacting Ignorant $
    profile "name" $
      return ()

sources :: Script Sources ()
sources =
  reacting Retry $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  reacting Abortive $
    link "source" "destination"
