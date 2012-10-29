{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Monad (when)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)

import Control.Lens
import Data.DeriveTH
import Data.Derive.Default ()
import Data.Default (Default(def))
import Options.Applicative
import System.FilePath ((</>))

import Biegunka
import Biegunka.Source.Git


data Settings = Settings
  { _directory ∷ FilePath
  , _tools ∷ Bool
  , _experimental ∷ Bool
  } deriving (Show, Read, Eq, Ord)


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


instance Default Bool where
  def = False


$(derive makeDefault ''Settings)
$(derive makeDefault ''Template)
$(derive makeDefault ''Xmobar)
$(derive makeDefault ''Xmonad)
$(derive makeDefault ''Urxvt)
$(derive makeDefault ''Xmodmap)


main ∷ IO ()
main = execParser opts >>= \(s,t) → commands s %
  pretend >-> executeWith (defaultExecution % templates .~ t) >-> verify
 where
  opts = info (helper <*> sample) (fullDesc <> header "Biegunka script")

  sample =
     flag def (laptopSettings, laptopTemplates) (long "laptop" <> short 'l' <> help "Use laptop settings") <|>
     flag def (workSettings, workTemplates) (long "work" <> short 'w' <> help "Use work settings")

  (>->) = liftA2 (>>)


laptopSettings, workSettings ∷ Settings
laptopSettings = def
  { _directory = "laptop"
  , _tools = True
  , _experimental = True
  }
workSettings = def
  { _directory = "work"
  }


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


commands ∷ Settings → Script Profile
commands Settings {..} = do
  profile "mine" $ do
    dotfiles
    when _tools tools
    "git@github.com:supki/zsh-cabal-completion" --> "git/zsh-cabal-completion"
  profile "vim" vim
  profile "misc" $ do
    "https://github.com/zsh-users/zsh-completions.git" --> "git/zsh-completions"
    "https://github.com/stepb/urxvt-tabbedex"          --> "git/urxvt-tabbedex"
  when _experimental . profile "experimental" $ do
    "https://github.com/sol/vimus"          --> "git/vimus"
    "https://github.com/sol/libmpd-haskell" --> "git/libmpd-haskell"
 where
  dotfiles = git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    ex link $ traverse . _1 %~ ("core" </>) $
      [ ("xsession", ".xsession")
      , ("mpdconf", ".mpdconf")
      , ("bashrc", ".bashrc")
      , ("zshrc", ".zshrc")
      , ("inputrc", ".inputrc")
      , ("profile", ".profile")
      , ("vimrc", ".vimrc")
      , ("ghci", ".ghci")
      , ("haskeline", ".haskeline")
      , ("racketrc", ".racketrc")
      , ("gitconfig", ".gitconfig")
      , ("gitignore", ".gitignore")
      , ("ackrc", ".ackrc")
      , ("vim/pathogen.vim", ".vim/autoload/pathogen.vim")
      , ("vim/cscope_maps.vim", ".vim/bundle/cscope_maps.vim")
      , ("vim/scratch", ".vim/bundle/scratch")
      , ("vim/indent/haskell.vim", ".vim/indent/haskell.vim")
      , ("conceal/haskell.vim", ".vim/after/syntax/haskell.vim")
      , ("XCompose", ".XCompose")
      , ("vimusrc", ".vimusrc")
      ]
    ex link $ traverse . _1 %~ ("extended" </>) $
      [ ("xmonad.hs", ".xmonad/xmonad.hs")
      , ("xmonad/Controls.hs", ".xmonad/lib/Controls.hs")
      , ("xmonad/Layouts.hs", ".xmonad/lib/Layouts.hs")
      , ("xmonad/Startup.hs", ".xmonad/lib/Startup.hs")
      , ("xmonad/Themes.hs", ".xmonad/lib/Themes.hs")
      , ("xmonad/Workspaces.hs", ".xmonad/lib/Workspaces.hs")
      , ("gvimrc", ".gvimrc")
      , ("vimcolors", ".vim/colors")
      , ("pentadactylrc", ".pentadactylrc")
      , ("pentadactyl/wanker.penta", ".pentadactyl/plugins/wanker.penta")
      , ("gtkrc.mine", ".gtkrc.mine")
      , ("xmobar.hs", ".xmobar/xmobar.hs")
      ]
    ex substitute $ traverse . _1 %~ ("extended" </>) $
      [ ("xmobarrc.template", ".xmobarrc")
      , ("xmonad/Misc.hs.template", ".xmonad/lib/Misc.hs")
      , ("xmodmap.template", ".xmodmap")
      , ("Xdefaults.template", ".Xdefaults")
      ]
    ex link $ traverse . _1 %~ (_directory </>) $
      [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
      ]

  tools = git "git@budueba.com:tools" "git/tools" $ do
    ex link
      [ ("youtube-in-mplayer.sh", "bin/youtube-in-mplayer")
      , ("cue2tracks.sh", "bin/cue2tracks")
      , ("weather.rb", "bin/ask-weather")
      , ("mpd/.lastfm.conf", ".lastfm.conf")
      , ("mpd/lastfm.png", ".icons/lastfm.png")
      , ("mpd/love.hs", "bin/lastfm-love-current-mpd-track")
      , ("trayicon/mcabber.py", "bin/trayicon-mcabber")
      , ("trayicon/icons/mcabber-default.png", ".icons/mcabber-default.png")
      , ("trayicon/icons/mcabber-unread.png", ".icons/mcabber-unread.png")
      , ("trayicon/mpd.py", "bin/trayicon-mpd")
      , ("trayicon/icons/mpd-pause.png", ".icons/mpd-pause.png")
      , ("trayicon/icons/mpd-playing.png", ".icons/mpd-playing.png")
      , ("battery.rb", "bin/vaio-battery")
      , ("upload/screenshot.sh", "bin/upload-screenshot")
      , ("upload/budueba.sh", "bin/upload-budueba")
      , ("upload/pastebin.hs", "bin/upload-pastebin")
      ]
    ignorant $ ex ghc
      [ ("mpd/scrobbler.hs", "bin/liblastfm-scrobbler")
      , ("audio.hs", "bin/vaio-audio")
      , ("shutdown-gui.hs", "bin/shutdown-gui")
      ]

  vim = do
    "git@github.com:Shougo/vimproc"               --> ".vim/bundle/vimproc"
    "git@github.com:eagletmt/ghcmod-vim"          --> ".vim/bundle/ghcmod-vim"
    "git@github.com:ujihisa/neco-ghc"             --> ".vim/bundle/neco-ghc"
    "git@github.com:Shougo/neocomplcache"         --> ".vim/bundle/neocomplcache"
    "git@github.com:spolu/dwm.vim"                --> ".vim/bundle/dwm"
    "git@github.com:vim-scripts/bufexplorer.zip"  --> ".vim/bundle/be"
    "git@github.com:rosstimson/scala-vim-support" --> ".vim/bundle/scala-vim-support"


ex ∷ Monad m ⇒ (FilePath → FilePath → m a) → [(FilePath, FilePath)] → m ()
ex = mapM_ . uncurry


(-->) ∷ String → FilePath → Script Source
(-->) = git_
