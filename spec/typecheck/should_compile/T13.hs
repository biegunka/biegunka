{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Check that biegunka mode is configurable
module OverloadedStrings where

import Control.Lens
import Control.Biegunka
import Data.Default (def)

main = biegunka (set mode Offline . set mode Online) def script
 where
  script = return ()
