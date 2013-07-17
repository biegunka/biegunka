{-# LANGUAGE DataKinds #-}
-- |
-- Checks that you cannot chain Actions
module Chaining where

import Control.Biegunka
import Control.Biegunka.Source.Git


chained_script_0 :: Script Actions ()
chained_script_0 =
  shell "echo hello"
 <~>
  shell "echo bye"

-- STDERR
--     Couldn't match type 'Actions with 'Sources
--     Expected type: Script 'Sources ()
--       Actual type: Script 'Actions ()
