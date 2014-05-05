{-# LANGUAGE DataKinds #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git
import System.Directory.Layout


main :: IO ()
main = return ()

sources :: Script Sources ()
sources =
  sudo (username "nobody") $
    git "https://example.com/dotfiles.git" "/" $
      return ()

sources' :: Script Sources ()
sources' =
  sudo (username "nobody") $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  sudo (uid 0) $
    link "source" "destination"
