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
  β ← load
  let γ = merge α β
  save γ

install = bzdury
  [ git "https://github.com/ujihisa/neco-ghc" "/home/maksenov/git/neco-ghc" --> neco_ghc
  , git "https://github.com/Shougo/neocomplcache" "/home/maksenov/git/neocomplcache" --> neocomplicache
  , git "https://github.com/zsh-users/zsh-completions.git" "/home/maksenov/git/zsh-completions" --> completions
  , git "git@github.com:supki/.dotfiles" "/home/maksenov/git/.dotfiles" --> dotfiles
  , git "git@github.com:supki/zsh-cabal-completion" "/home/maksenov/git/zsh-cabal-completion" --> cabal_completion
  ]
  where neco_ghc = do
          message "Installing neco-ghc"
          link_repo_itself ".vim/bundle/neco-ghc"
        neocomplicache = do
          message "Installing neocomplcache"
          link_repo_itself ".vim/bundle/neocomplcache"
        cabal_completion =
          message "Installing zsh cabal completion"
        completions =
          message "Installing zsh completions"

{-
 - .dotfiles
 -}

data Set = C | E | W
           deriving Show

dir C = "core"
dir E = "extended"
dir W = "work"

dotfiles ∷ Script ()
dotfiles = mapM_ installSet [C, E, W]
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
