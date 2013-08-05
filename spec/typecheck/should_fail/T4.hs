{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Template haskell tests
--
-- Checks you cannot get convenient option parser from complex data declarations
module TH where

import Control.Biegunka


data Env = A Int | B Int

biegunkaOptions ''Env


-- STDERR
--     Couldn't match expected type `Env' with actual type `Int -> Env'
--
