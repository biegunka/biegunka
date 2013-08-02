{-# LANGUAGE DataKinds #-}
-- |
-- Checks that reacting works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = return ()

sources :: Script Sources ()
sources =
  reacting Ignorant $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  reacting Abortive $
    link "source" "destination"
