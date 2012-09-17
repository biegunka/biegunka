{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
import Control.Monad (when)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.Typeable (Typeable)

import Control.Lens
import Control.Monad.Reader (local)
import Data.Default (Default(def))
import Options.Applicative
import System.FilePath.Lens ((</>~))

import Biegunka
import Biegunka.Source.Git


data Settings = Settings
  { _profileDirectory ∷ FilePath
  , _buildTools ∷ Bool
  , _buildExperimental ∷ Bool
  } deriving (Show, Read, Eq, Ord)


makeLenses ''Settings


data Template = Template
  { xmobar ∷ Xmobar
  , xmonad ∷ Xmonad
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


main ∷ IO ()
main = execParser opts >>= evaluate
 where
   opts = info (helper <*> sample) (fullDesc <> header "Biegunka script")

   sample =
     flag def (laptopSettings, laptopTemplates) (long "laptop" <> short 'l' <> help "Use laptop settings") <|>
     flag def (workSettings, workTemplates) (long "work" <> short 'w' <> help "Use work settings")


instance Default Settings where
  def = Settings
    { _profileDirectory = ""
    , _buildTools = False
    , _buildExperimental = False
    }


laptopSettings ∷ Settings
laptopSettings = def
    { _profileDirectory = "laptop"
    , _buildTools = True
    , _buildExperimental = True
    }


workSettings ∷ Settings
workSettings = def
    { _profileDirectory = "work"
    }


instance Default Template where
  def = Template
    { xmobar = def
    , xmonad = def
    }


instance Default Xmobar where
  def = Xmobar
    { background = def
    , position = def
    , battery = def
    }


instance Default Xmonad where
  def = Xmonad
    { terminal = def
    , ubuntu = def
    , terminus = def
    , white = def
    , grayDark = def
    , grayLight = def
    , black = def
    , orange = def
    , yellow = def
    }


laptopTemplates ∷ Template
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
  }


workTemplates ∷ Template
workTemplates = def
  { xmobar = def
      { background = "\"#333333\""
      , position = "Static { xpos = 102, ypos = 750, width = 1264, height = 20 }"
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
  }


evaluate ∷ (Settings, Template) → IO ()
evaluate = (pretend >>> execute >>> verify) . commands
 where
  (>>>) = liftA2 (>>)


commands ∷ (Settings, Template) → ProfileScript Settings Template ()
commands (settings, templates) = do
  custom .= settings
  template .= templates
  profile "mine" $ do
    dotfiles
    whenM (use $ custom . buildTools) tools
    git_ "git@github.com:supki/zsh-cabal-completion" "git/zsh-cabal-completion"

  profile "vim" vim

  profile "misc" $ do
    git_ "https://github.com/zsh-users/zsh-completions.git" "git/zsh-completions"
    git_ "https://github.com/stepb/urxvt-tabbedex" "git/urxvt-tabbedex"

  whenM (use $ custom . buildExperimental) $
    profile "experimental" $ do
      git_ "https://github.com/sol/vimus" "git/vimus"
      git_ "https://github.com/sol/libmpd-haskell" "git/libmpd-haskell"
 where
  dotfiles = git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
    local (sourceRoot </>~ "core") $ do
      mapM_ (uncurry link)
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
    local (sourceRoot </>~ "extended") $ do
      mapM_ (uncurry link)
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
      mapM_ (uncurry substitute)
        [ ("xmobarrc.template", ".xmobarrc")
        , ("xmonad/Misc.hs.template", ".xmonad/lib/Misc.hs")
        ]
    directory ← query $ custom . profileDirectory
    local (sourceRoot </>~ directory) $ do
      mapM_ (uncurry link)
        [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
        , ("Xdefaults", ".Xdefaults")
        , ("xmodmap", ".xmodmap")
        ]

  tools = git "git@budueba.com:tools" "git/tools" $ do
    mapM_ (uncurry link)
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
    mapM_ (uncurry $ compile GHC)
      [ ("mpd/scrobbler.hs", "bin/liblastfm-scrobbler")
      , ("audio.hs", "bin/vaio-audio")
      , ("shutdown-gui.hs", "bin/shutdown-gui")
      ]

  vim = do
    git "https://github.com/Shougo/vimproc" "git/vimproc" $ registerAt ".vim/bundle/vimproc"
    git "https://github.com/eagletmt/ghcmod-vim" "git/ghcmod-vim" $ registerAt ".vim/bundle/ghcmod-vim"
    git "https://github.com/ujihisa/neco-ghc" "git/neco-ghc" $ registerAt ".vim/bundle/neco-ghc"
    git "https://github.com/Shougo/neocomplcache" "git/neocomplcache" $ registerAt ".vim/bundle/neocomplcache"
    git "https://github.com/spolu/dwm.vim" "git/dwm" $ registerAt ".vim/bundle/dwm"
    git "https://github.com/vim-scripts/bufexplorer.zip" "git/vim-be" $ registerAt ".vim/bundle/be"


whenM ∷ Monad m ⇒ m Bool → m () → m ()
whenM ma mb = ma >>= \p → when p mb
