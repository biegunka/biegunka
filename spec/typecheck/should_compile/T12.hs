{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Check that -XOverloadedStrings does not hinder compilation
module OverloadedStrings where

import Control.Biegunka
import Control.Biegunka.Source.Directory


some_profile :: Script Sources ()
some_profile = do
  profile "main" $
    directory "/home/user/playground" $
      link "vimrc" ".vimrc"
  profile "sudoed" $
    sudo "root" $ directory "/home/user/playground" $
      link "xmonad.hs" ".xmonad/xmonad.hs"
