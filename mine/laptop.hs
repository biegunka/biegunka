#!/usr/bin/env runhaskell
{-# LANGUAGE UnicodeSyntax #-}

import Biegunka
import Control.Arrow (first)
import Control.Monad (forM_)
import Data.Monoid ((<>))
import System.FilePath ((</>))

main ∷ IO ()
main = do
  α ← install
  withBiegunka (return . merge α)

install = bzdury
  [ git "https://github.com/Shougo/vimproc" "/home/maksenov/git/vimproc" --> vimproc
  , git "https://github.com/eagletmt/ghcmod-vim" "/home/maksenov/git/ghcmod-vim" --> ghcmod_vim
  , git "https://github.com/ujihisa/neco-ghc" "/home/maksenov/git/neco-ghc" --> neco_ghc
  , git "https://github.com/Shougo/neocomplcache" "/home/maksenov/git/neocomplcache" --> neocomplicache
  , git "https://github.com/zsh-users/zsh-completions.git" "/home/maksenov/git/zsh-completions" --> completions
  , git "https://github.com/stepb/urxvt-tabbedex" "/home/maksenov/git/urxvt-tabbedex" --> tabbedex
  , git "git@github.com:supki/.dotfiles" "/home/maksenov/git/.dotfiles" --> dotfiles
  , git "git@github.com:supki/zsh-cabal-completion" "/home/maksenov/git/zsh-cabal-completion" --> cabal_completion
  , git "git@budueba.com:tools" "/home/maksenov/git/tools" --> utils
  ]
 where
  vimproc =
    do message "Installing vimproc"
       link_repo_itself ".vim/bundle/vimproc"
  ghcmod_vim =
    do message "Installing ghcmod-vim"
       link_repo_itself ".vim/bundle/ghcmod-vim"
  neco_ghc =
    do message "Installing neco-ghc"
       link_repo_itself ".vim/bundle/neco-ghc"
  neocomplicache =
    do message "Installing neocomplcache"
       link_repo_itself ".vim/bundle/neocomplcache"
  cabal_completion = message "Installing zsh cabal completion"
  completions = message "Installing zsh completions"
  tabbedex = message "Installing urxvt-tabbedex"

{-
 - .dotfiles
 -}

data Set = C | E | L
           deriving Show

dir C = "core"
dir E = "extended"
dir L = "laptop"

dotfiles ∷ Script ()
dotfiles = mapM_ installSet [C, E, L]
  where installSet s = do
          message $ "Installing " <> show s <> " configs..."
          forM_ (links s) $ uncurry link_repo_file . first (dir s </>)
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

{-
 - utils
 -}

utils = do
  message "Installing tools"
  forM_ links $ uncurry link_repo_file
  forM_ execs $ uncurry (compile_with GHC)
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
