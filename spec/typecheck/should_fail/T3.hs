-- |
-- Checks that profiles scope cannot be nested
module Main where

import Biegunka
import Biegunka.Source.Git


main :: IO ()
main = biegunka id (execute id) $ do
  profile "outer" $
    profile "inner" $
      return ()

-- STDERR
-- Couldn't match type 'Profiles with 'Sources
--     Expected type: Script 'Sources ()
--       Actual type: Script 'Profiles ()
