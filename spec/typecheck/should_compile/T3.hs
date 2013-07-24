{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Checks that sudo works in any scope
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = return ()

sources :: Script Sources ()
sources =
  sudo "nobody" $
    git "https://example.com/dotfiles.git" "/" $
      return ()

actions :: Script Actions ()
actions =
  sudo (UserID 0) $
    link "source" "destination"

actions' :: Script Actions ()
actions' =
  sudo (Username "nobody") $
    link "source" "destination"
