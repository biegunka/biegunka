{-# LANGUAGE DataKinds #-}
-- |
-- Chaining tests
--
-- Checks you /can/ declare tasks prerequisites of another
module Chaining where

import Control.Biegunka
import Control.Biegunka.Source.Directory


chained_script_0 :: Script 'Sources ()
chained_script_0 =
  directory "/" pass
 `prerequisiteOf`
  directory "/" pass

chained_script_1 :: Script 'Sources ()
chained_script_1 =
  directory "/" pass
 <~>
  directory "/" pass

chained_script_2 :: Script 'Sources ()
chained_script_2 =
  directory "/" pass
 <~>
  directory "/" pass
 <~>
  directory "/" pass

chained_script_3 :: Script 'Sources ()
chained_script_3 = do
  directory "/" pass
  directory "/" pass
 <~>
  directory "/" pass

chained_script_4 :: Script 'Sources ()
chained_script_4 = do
  directory "/" pass
  directory "/" pass
 <~> do
  directory "/" pass
  directory "/" pass
