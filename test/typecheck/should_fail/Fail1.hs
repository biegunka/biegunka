-- |
-- Checks that actions scope cannot be top-level
module Main where

import Control.Monad (void)
import Control.Biegunka


main :: IO ()
main = void (biegunka id run (link ".xmonad/xmonad.hs" ".xmonad/xmonad.hs.bak"))

-- STDERR
--     Couldn't match type ‘'Actions’ with ‘'Sources’
--     Expected type: Script 'Sources ()
--       Actual type: Script 'Actions ()
