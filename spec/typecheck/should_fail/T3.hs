{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Template haskell tests
--
-- Checks you cannot get convenient option parser from complex data declarations
module TH where

import           Biegunka


data Env = A Int | B Double

makeOptionsParser ''Env


-- STDERR
--     Couldn't match type `Double' with `Int'
--
