{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
import Control.Applicative (liftA2)
import Control.Monad (when)

import Control.Lens
import Control.Monad.State (get, mapStateT)
import Data.Default (Default(def))
import System.FilePath.Lens ((</>=))

import Biegunka
import Biegunka.Source.Git
import Biegunka.Source.Tar


data Custom = Custom
  { _profileDirectory ∷ FilePath
  , _buildTools ∷ Bool
  , _buildExperimental ∷ Bool
  } deriving (Show, Read, Eq, Ord)


makeLenses ''Custom


instance Default Custom where
  def = Custom
    { _profileDirectory = "laptop"
    , _buildTools = True
    , _buildExperimental = True
    }


main ∷ IO ()
main = pretend >>> execute >>> verify $ script
 where
  script = do
    profile "mine" $ do
      dotfiles
      whenM (use $ custom . buildTools) tools
      git_ "git@github.com:supki/zsh-cabal-completion" "git/zsh-cabal-completion"

    profile "vim" $ vim

    profile "misc" $ do
      git_ "https://github.com/zsh-users/zsh-completions.git" "git/zsh-completions"
      git_ "https://github.com/stepb/urxvt-tabbedex" "git/urxvt-tabbedex"

    whenM (use $ custom . buildExperimental) $
      profile "experimental" $ do
        git_ "https://github.com/sol/vimus" "git/vimus"
        git_ "https://github.com/sol/libmpd-haskell" "git/libmpd-haskell"

  (>>>) = liftA2 (>>)

  whenM ma mb = ma >>= \p → when p mb


dotfiles ∷ SourceScript Custom ()
dotfiles = git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
  localStateT $ do
    sourceRoot </>= "core"
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
  localStateT $ do
    sourceRoot </>= "extended"
    mapM_ (uncurry link)
      [ ("xmonad.hs", ".xmonad/xmonad.hs")
      , ("xmonad/Controls.hs", ".xmonad/lib/Controls.hs")
      , ("xmonad/Layouts.hs", ".xmonad/lib/Layouts.hs")
      , ("xmonad/Misc.hs", ".xmonad/lib/Misc.hs")
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
  localStateT $ do
    directory ← use $ custom . profileDirectory
    sourceRoot </>= directory
    mapM_ (uncurry link)
      [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
      , ("mcabberrc", ".mcabberrc")
      , ("ncmpcpp", ".ncmpcpp/config")
      , ("xmobarrc", ".xmobarrc")
      , ("Xdefaults", ".Xdefaults")
      , ("xmodmap", ".xmodmap")
      ]
 where
  localStateT m = get >>= \s → mapStateT (>> return ((), s)) m


tools ∷ SourceScript Custom ()
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


vim ∷ SourceScript Custom ()
vim = do
  git "https://github.com/Shougo/vimproc" "git/vimproc" $ registerAt ".vim/bundle/vimproc"
  git "https://github.com/eagletmt/ghcmod-vim" "git/ghcmod-vim" $ registerAt ".vim/bundle/ghcmod-vim"
  git "https://github.com/ujihisa/neco-ghc" "git/neco-ghc" $ registerAt ".vim/bundle/neco-ghc"
  git "https://github.com/Shougo/neocomplcache" "git/neocomplcache" $ registerAt ".vim/bundle/neocomplcache"
  git "https://github.com/spolu/dwm.vim" "git/dwm" $ registerAt ".vim/bundle/dwm"
  git "https://github.com/widox/vim-buffer-explorer-plugin" "git/vim-be" $ registerAt ".vim/bundle/be"
