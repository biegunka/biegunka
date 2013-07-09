-- |
-- Checks that profiles (and groups) /can/ be nested
module Main where

import Biegunka


main :: IO ()
main = biegunka id (run id) $ do
  profile "outer" $
    profile "inner" $
      return ()
  profile "outer" $
    group "inner" $
      return ()
