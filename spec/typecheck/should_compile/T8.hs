{-# LANGUAGE DataKinds #-}
-- |
-- Chaining tests
--
-- Checks you /can/ declare tasks prerequisites of another
module Chaining where

import Biegunka
import Biegunka.Source.Directory
import Data.Default (def)


chained_script_0 :: Script Sources ()
chained_script_0 =
  directory "/" def
 `prerequisiteOf`
  directory "/" def

chained_script_1 :: Script Sources ()
chained_script_1 =
  directory "/" def
 <~>
  directory "/" def

chained_script_2 :: Script Sources ()
chained_script_2 =
  directory "/" def
 <~>
  directory "/" def
 <~>
  directory "/" def

chained_script_3 :: Script Sources ()
chained_script_3 = do
  directory "/" def
  directory "/" def
 <~>
  directory "/" def

chained_script_4 :: Script Sources ()
chained_script_4 = do
  directory "/" def
  directory "/" def
 <~> do
  directory "/" def
  directory "/" def
