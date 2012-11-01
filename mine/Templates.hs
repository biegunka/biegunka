{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Templates (laptopTemplates, workTemplates) where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.DeriveTH
import Data.Default (Default(def))
import Data.Derive.Default ()


data Template = Template
  { xmobar ∷ Xmobar
  , xmonad ∷ Xmonad
  , xmodmap ∷ Xmodmap
  , urxvt ∷ Urxvt
  } deriving (Data, Typeable)


data Xmobar = Xmobar
  { background ∷ String
  , position ∷ String
  , battery ∷ Maybe String
  } deriving (Data, Typeable)


data Xmonad = Xmonad
  { terminal ∷ String
  , ubuntu ∷ String
  , terminus ∷ String
  , white ∷ String
  , grayDark ∷ String
  , grayLight ∷ String
  , black ∷ String
  , orange ∷ String
  , yellow ∷ String
  } deriving (Data, Typeable)


data Xmodmap = Xmodmap
  { menu ∷ String
  } deriving (Data, Typeable)


data Urxvt = Urxvt
  { tabbedex ∷ String
  , background_ ∷ String
  , browser ∷ String
  } deriving (Data, Typeable)


$(derive makeDefault ''Template)
$(derive makeDefault ''Xmobar)
$(derive makeDefault ''Xmonad)
$(derive makeDefault ''Urxvt)
$(derive makeDefault ''Xmodmap)


laptopTemplates, workTemplates ∷ Template
laptopTemplates = def
  { xmobar = def
    { background = "\"#333333\""
    , position = "Static { xpos = 102, ypos = 750, width = 1264, height = 20 }"
    , battery = Just "%battery%%mysep%"
    }
  , xmonad = def
    { terminal = "urxvtcd"
    , ubuntu = "xft:ubuntu:size=9"
    , terminus = "xft:terminus:size=9"
    , white = "#ffffff"
    , grayDark = "#474747"
    , grayLight = "#cccccc"
    , black = "#333333"
    , orange = "#dd9977"
    , yellow = "#eeccaa"
    }
  , xmodmap = def
    { menu = "keysym Menu = Super_R"
    }
  , urxvt = def
    { tabbedex = "/home/maksenov/git/urxvt-tabbedex"
    , background_ = "#333333"
    , browser = "iceweasel"
    }
  }
workTemplates = def
  { xmobar = def
    { background = "\"#373737\""
    , position = "BottomW R 94"
    }
  , xmonad = def
    { terminal = "urxvt"
    , ubuntu = "xft:ubuntu:size=9"
    , terminus = "xft:terminus:size=9"
    , white = "#ffffff"
    , grayDark = "#515151"
    , grayLight = "#cccccc"
    , black = "#373737"
    , orange = "#dd9977"
    , yellow = "#eeccaa"
    }
  , urxvt = def
    { tabbedex = "/home/pyoseek/git/urxvt-tabbedex"
    , background_ = "#373737"
    , browser = "firefox"
    }
  }
