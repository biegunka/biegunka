{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- Check that -XOverloadedStrings does not break compilation
module OverloadedStrings where

import Control.Biegunka
import Control.Biegunka.Source.Directory


a_script :: Script Sources ()
a_script = do
  namespace "main" $
    directory "/home/user/playground" $
      link "vimrc" ".vimrc"
  namespace "sudoed" $
    sudo "root" $ directory "/home/user/playground" $
      link "xmonad.hs" ".xmonad/xmonad.hs"
