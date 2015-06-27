-- |
-- Check that biegunka mode is configurable
module OverloadedStrings where

import Control.Biegunka
import Control.Lens
import Data.Monoid (mempty)
import System.Exit (ExitCode)


main :: IO ExitCode
main = biegunka (set mode Offline . set mode Online) mempty script
 where
  script = return ()
