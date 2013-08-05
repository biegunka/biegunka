{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Template haskell tests
--
-- Checks you /can/ get convenient option parser from simple data declarations
module TH where

import Control.Biegunka


data Env = A | B

biegunkaOptions ''Env
