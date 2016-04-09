-- |
-- Check that biegunka mode is configurable
module OverloadedStrings where

import Control.Biegunka
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka (offline . online) mempty script
 where
  script = return ()
