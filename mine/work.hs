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
      git "git@github.com:supki/zsh-cabal-completion" "git/zsh-cabal-completion" $
        return ()

    profile "vim-related" $ do
      git "https://github.com/ujihisa/neco-ghc" "git/neco-ghc" $ do
        registerAt ".vim/bundle/neco-ghc"
      git "https://github.com/Shougo/neocomplcache" "git/neocomplcache" $ do
        registerAt ".vim/bundle/neocomplcache"
      git "https://github.com/spolu/dwm.vim" "git/dwm" $
        registerAt ".vim/bundle/dwm"

    profile "misc" $ do
      git "https://github.com/zsh-users/zsh-completions.git" "git/zsh-completions" $
        return ()
      git "https://github.com/stepb/urxvt-tabbedex" "git/urxvt-tabbedex" $
        return ()

  (|>>|) = liftA2 (>>)


dotfiles ∷ SourceScript ()
dotfiles = git "git@github.com:supki/.dotfiles" "git/dotfiles" $ do
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
      , ("vim/haskellmode.vim", ".vim/autoload/haskellmode.vim")
      , ("vim/cscope_maps.vim", ".vim/bundle/cscope_maps.vim")
      , ("vim/scratch", ".vim/bundle/scratch")
      , ("conceal/haskell.vim", ".vim/after/syntax/haskell.vim")
      , ("XCompose", ".XCompose")
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
      , ("gtkrc.mine", ".gtkrc.mine")
      , ("xmobar.hs", ".xmobar/xmobar.hs")
      ]
  localStateT $ do
    repositoryRoot </>= "work"
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
