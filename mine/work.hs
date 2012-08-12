{-# LANGUAGE UnicodeSyntax #-}

import Control.Arrow (first)
import Control.Monad (forM_)
import Control.Monad.Free (Free)

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Biegunka


main ∷ IO ()
main = do
  hd ← getHomeDirectory
  execute $ do
    profile "mine" $ do
      git "git@github.com:supki/.dotfiles" (hd </> "git/dotfiles")
        dotfiles
      git "git@github.com:supki/zsh-cabal-completion" (hd </> "git/zsh-cabal-completion") $
        return ()
    profile "vim-related" $ do
      git "https://github.com/ujihisa/neco-ghc" (hd </> "git/neco-ghc") $ do
        registerAt ".vim/bundle/neco-ghc"
      git "https://github.com/Shougo/neocomplcache" (hd </> "git/neocomplcache") $ do
        registerAt ".vim/bundle/neocomplcache"
    profile "misc" $ do
      git "https://github.com/zsh-users/zsh-completions.git" (hd </> "git/zsh-completions") $
        return ()
      git "https://github.com/stepb/urxvt-tabbedex" (hd </> "git/urxvt-tabbedex") $
        return ()


data Set = C | E | W
           deriving Show


dir ∷ Set → String
dir C = "core"
dir E = "extended"
dir W = "work"


dotfiles ∷ Free Script ()
dotfiles = mapM_ installSet [C, E, W]
  where installSet s = do
          message $ "Installing " ++ show s ++ " configs..."
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
          , ("vim/haskellmode.vim", ".vim/autoload/haskellmode.vim")
          , ("vim/cscope_maps.vim", ".vim/bundle/cscope_maps.vim")
          , ("vim/scratch", ".vim/bundle/scratch")
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
          , ("gtkrc.mine", ".gtkrc.mine")
          , ("xmobar.hs", ".xmobar/xmobar.hs")
          ]
        links W =
          [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
          , ("mcabberrc", ".mcabberrc")
          , ("ncmpcpp", ".ncmpcpp/config")
          , ("xmobarrc", ".xmobarrc")
          , ("Xdefaults", ".Xdefaults")
          , ("xmodmap", ".xmodmap")
          ]
