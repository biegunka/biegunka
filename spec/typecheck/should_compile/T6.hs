-- |
--
-- Checks that namespaces /can/ be nested
module Main where

import Control.Biegunka
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka id run $ do
  namespace "outer" $
    namespace "inner" $
      return ()
  namespace "outer" $
    namespace "inner" $
      return ()
