{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
module Templates (laptopTemplates, workTemplates) where

import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.DeriveTH
import Data.Default (Default(def))
import Data.Derive.Default ()
import Data.String.Quote


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
  , startup ∷ String
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
    , startup =
      [s|
        [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 102 --heighttype pixel --height 18 --tint 0x" ++ tail blackColor ++ " --alpha 0")
        , ("/home/maksenov/.dropbox-dist/dropbox", "${HOME}/.dropbox-dist/dropboxd")
        , ("mpd", "mpd")
        , ("/usr/lib/iceweasel", "iceweasel")
        , ("gnome-commander", "gnome-commander")
        , ("transmission-gtk", "transmission-gtk")
        , ("ssh .* mcabber", "urxvtcd -title mcabber -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t mcabber'")
        , ("ssh .* irssi", "urxvtcd -title irssi -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t irssi'")
        , ("watch -n2 netstat", "urxvtcd -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvtcd -title htop -e htop")
        , ("liblastfm-scrobbler", "liblastfm-scrobbler")
        , ("procfiled", "procfiled")
        , ("icedove", "icedove")
        , ("python /home/maksenov/bin/trayicon-mpd", "trayicon-mpd")
        , ("python /home/maksenov/bin/trayicon-mcabber", "trayicon-mcabber")
        , ("suspender", "suspender")
        ]
      |]
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
    , startup =
      [s|
        [ ("trayer", "trayer --edge bottom --align left --transparent true --widthtype pixel --width 78 --heighttype pixel --height 17 --tint 0x" ++ tail blackColor ++ " --alpha 0")
        , ("/usr/lib/firefox/firefox", "firefox")
        , ("thunderbird", "thunderbird")
        , ("gnome-commander", "gnome-commander")
        , ("ssh .* mcabber", "urxvt -title mcabber -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t mcabber'")
        , ("ssh .* irssi", "urxvt -title irssi -e ssh matt@budueba.com -t 'export LANG=en_US.UTF-8; tmux attach -t irssi'")
        , ("watch -n2 netstat", "urxvt -title netstat -e watch -n2 netstat -anptu | egrep '^Proto|:80' | sort")
        , ("htop", "urxvt -title htop -e htop")
        ]
      |]
    }
  , urxvt = def
    { tabbedex = "/home/pyoseek/git/urxvt-tabbedex"
    , background_ = "#373737"
    , browser = "firefox"
    }
  }
