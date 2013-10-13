-- |
-- Checks that profiles (and groups) /can/ be nested
module Main where

import Control.Biegunka


main = biegunka id run $ do
  profile "outer" $
    profile "inner" $
      return ()
  profile "outer" $
    group "inner" $
      return ()
