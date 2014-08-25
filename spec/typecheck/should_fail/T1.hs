-- |
-- Checks that actions scope cannot be top-level
module Main where

import Control.Applicative
import Control.Biegunka


main :: IO ()
main = () <$ biegunka id run $ do
  link ".xmonad/xmonad.hs" ".xmonad/xmonad.hs.bak"

-- STDERR
--     Couldn't match type ‘'Actions’ with ‘'Sources’
--     Expected type: Script 'Actions ()
--       Actual type: Script 'Sources ()
