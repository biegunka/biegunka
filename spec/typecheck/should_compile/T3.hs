{-# LANGUAGE DataKinds #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = return ()

sources :: Script Sources ()
sources =
  sudo (Username "nobody") $
    git "https://example.com/dotfiles.git" "/" $
      return ()

sources' :: Script Sources ()
sources' =
  sudo (Username "nobody") $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  sudo 0 $
    link "source" "destination"
