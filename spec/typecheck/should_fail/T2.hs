{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Checks that you cannot chain Actions
module Chaining where

import Control.Biegunka
import Control.Biegunka.Source.Git


chained_script_0 :: Script Actions ()
chained_script_0 =
  [sh| echo hello |]
 <~>
  [sh| echo bye |]

-- STDERR
--     Couldn't match type 'Sources with 'Actions
--     Expected type: Script 'Actions ()
--       Actual type: Script 'Sources ()
