{-# LANGUAGE UnicodeSyntax #-}
import Control.Applicative (liftA2)

import Control.Monad.State (get, mapStateT)
import System.FilePath.Lens ((</>=))

import Biegunka


main ∷ IO ()
main = execute |>>| verify $ script
 where
  script = do
    profile "mine" $ do
      dotfiles
      tools
      git "git@github.com:supki/zsh-cabal-completion" "git/zsh-cabal-completion" $
        return ()
    profile "vim-related" $ do
      git "https://github.com/Shougo/vimproc" "git/vimproc" $
        registerAt ".vim/bundle/vimproc"
      git "https://github.com/eagletmt/ghcmod-vim" "git/ghcmod-vim" $
        registerAt ".vim/bundle/ghcmod-vim"
      git "https://github.com/ujihisa/neco-ghc" "git/neco-ghc" $
        registerAt ".vim/bundle/neco-ghc"
      git "https://github.com/Shougo/neocomplcache" "git/neocomplcache" $
        registerAt ".vim/bundle/neocomplcache"
    profile "misc" $ do
      git "https://github.com/zsh-users/zsh-completions.git" "git/zsh-completions" $
        return ()
      git "https://github.com/stepb/urxvt-tabbedex" "git/urxvt-tabbedex" $
        return ()
    profile "experimental" $ do
      git "https://github.com/sol/vimus" "git/vimus" $
        return ()
      git "https://github.com/sol/libmpd-haskell" "git/libmpd-haskell" $
        return ()

  (|>>|) = liftA2 (>>)


dotfiles ∷ Script Source ()
dotfiles = git "git@github.com:supki/.dotfiles" "git/.dotfiles" $ do
  localStateT $ do
    repositoryRoot </>= "core"
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
    repositoryRoot </>= "extended"
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
    repositoryRoot </>= "laptop"
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


tools ∷ Script Source ()
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
