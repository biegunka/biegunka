-- |
-- Checks that profiles (and groups) /can/ be nested
module Main where

import Control.Biegunka
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka id run $ do
  profile "outer" $
    profile "inner" $
      return ()
  profile "outer" $
    group "inner" $
      return ()
