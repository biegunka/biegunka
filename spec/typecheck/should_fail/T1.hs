-- |
-- Checks that actions scope cannot be top-level
module Main where

import Control.Biegunka
import Control.Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  link ".xmonad/xmonad.hs" ".xmonad/xmonad.hs.bak"

-- STDERR
--     Couldn't match type 'Actions with 'Sources
--     Expected type: Script 'Sources ()
--       Actual type: Script 'Actions ()
