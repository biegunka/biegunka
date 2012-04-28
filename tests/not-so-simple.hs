import Biegunka.DryRun
import Control.Arrow (first)
import Control.Monad (forM_)
import System.FilePath ((</>))

links = git "https://github.com/supki/utils" "/home/maksenov/git/utils" --> utils [Core, Extended, Laptop]

data Set = Core
         | Extended
         | Laptop
         | Work
           deriving Show

dir Core = "core"
dir Extended = "extended"
dir Laptop = "laptop"
dir Work = "work"

utils ∷ [Set] → Script ()
utils xs = mapM_ installSet xs

installSet ∷ Set → Script ()
installSet s = do message $ "Installing " ++ show s ++ " configs..."
                  forM_ (pairs s) $ uncurry link_repo_file . first (dir s </>)

pairs ∷ Set → [(FilePath, FilePath)]
pairs Core =
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
  , ("ackrc", ".ackrc")
  , ("vim/pathogen.vim", ".vim/autoload/pathogen.vim")
  , ("vim/haskellmode.vim", ".vim/autoload/haskellmode.vim")
  , ("vim/cscope_maps.vim", ".vim/bundle/cscope_maps.vim")
  , ("vim/scratch", ".vim/bundle/scratch")
  , ("conceal/haskell.vim", ".vim/after/syntax/haskell.vim")
  , ("XCompose", ".XCompose")
  ]
pairs Extended =
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
  ]
pairs _ =
  [ ("xmonad/Profile.hs", ".xmonad/lib/Profile.hs")
  , ("mcabberrc", ".mcabberrc")
  , ("ncmpcpp", ".ncmpcpp/config")
  , ("xmobarrc", ".xmobarrc")
  , ("Xdefaults", ".Xdefaults")
  , ("xmodmap", ".xmodmap")
  ]
