import Biegunka
import Biegunka.Script.DryRun

links = bzdury 
  [ git "https://github.com/ujihisa/neco-ghc" "/home/maksenov/git/neco-ghc" --> neco_ghc
  , git "https://github.com/Shougo/neocomplcache" "/home/maksenov/git/neocomplcache" --> neocomplicache
  ] 
  where neco_ghc = link_repo_itself ".vim/bundle/neco-ghc"
        neocomplicache = link_repo_itself ".vim/bundle/neocomplcache"
