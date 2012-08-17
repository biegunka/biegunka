{-# LANGUAGE UnicodeSyntax #-}

import Control.Arrow (first)
import Control.Monad (forM_)
import Data.Monoid ((<>))

import System.FilePath ((</>))

import Biegunka


main ∷ IO ()
main = execute $ do
  profile "mine" $ do
    git "git@github.com:supki/.dotfiles" "/home/maksenov/git/.dotfiles"
      dotfiles
    git "git@github.com:supki/zsh-cabal-completion" "/home/maksenov/git/zsh-cabal-completion" $
      return ()
    git "git@budueba.com:tools" "/home/maksenov/git/tools"
      utils
  profile "vim-related" $ do
    git "https://github.com/Shougo/vimproc" "/home/maksenov/git/vimproc" $ do
      registerAt ".vim/bundle/vimproc"
    git "https://github.com/eagletmt/ghcmod-vim" "/home/maksenov/git/ghcmod-vim" $ do
      registerAt ".vim/bundle/ghcmod-vim"
    git "https://github.com/ujihisa/neco-ghc" "/home/maksenov/git/neco-ghc" $ do
      registerAt ".vim/bundle/neco-ghc"
    git "https://github.com/Shougo/neocomplcache" "/home/maksenov/git/neocomplcache" $ do
      registerAt ".vim/bundle/neocomplcache"
  profile "misc" $ do
    git "https://github.com/zsh-users/zsh-completions.git" "/home/maksenov/git/zsh-completions" $
      return ()
    git "https://github.com/stepb/urxvt-tabbedex" "/home/maksenov/git/urxvt-tabbedex" $
      return ()
  profile "experimental" $ do
    git "https://github.com/sol/vimus" "/home/maksenov/git/vimus" $
      return ()
    git "https://github.com/sol/libmpd-haskell" "/home/maksenov/git/libmpd-haskell" $
      return ()


data Set = C | E | L
           deriving Show


dir ∷ Set → String
dir C = "core"
dir E = "extended"
dir L = "laptop"


dotfiles ∷ Script Files ()
dotfiles = mapM_ installSet [C, E, L]
  where installSet s = do
          forM_ (links s) $ uncurry link . first (dir s </>)
        links C =
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
          ]
        links E =
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
        links L =
          [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
          , ("mcabberrc", ".mcabberrc")
          , ("ncmpcpp", ".ncmpcpp/config")
          , ("xmobarrc", ".xmobarrc")
          , ("Xdefaults", ".Xdefaults")
          , ("xmodmap", ".xmodmap")
          ]


utils ∷ Script Files ()
utils = do
  forM_ links $ uncurry link
  forM_ execs $ uncurry (compile GHC)
  where links =
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
        execs =
          [ ("mpd/scrobbler.hs", "bin/liblastfm-scrobbler")
          , ("audio.hs", "bin/vaio-audio")
          , ("shutdown-gui.hs", "bin/shutdown-gui")
          ]
