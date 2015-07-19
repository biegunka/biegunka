{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Check that -XOverloadedStrings does not break compilation
module OverloadedStrings where

import Control.Biegunka
import Control.Biegunka.Source.Directory


script :: Script 'Sources ()
script = do
  namespace "main" $
    directory "/home/user/playground" $
      link (origin "vimrc" . path ".vimrc")
  namespace "sudoed" $
    sudo $ directory "/home/user/playground" $
      link (origin "xmonad.hs" . path ".xmonad/xmonad.hs")
