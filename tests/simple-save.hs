{-# LANGUAGE BangPatterns #-}
import Control.Applicative ((<$>))
import Biegunka.DryRun
import System.FilePath ((<.>))
import System.Directory (renameFile)

link_one = git "https://github.com/supki/utils" "/home/maksenov/git/utils" --> utils
  where utils = link_repo_itself "sandbox/utils-link"

link_two = bzdury
  [ git "https://github.com/ujihisa/neco-ghc" "/home/maksenov/git/neco-ghc" --> neco_ghc
  , git "https://github.com/Shougo/neocomplcache" "/home/maksenov/git/neocomplcache" --> neocomplicache
  ]
  where neco_ghc = link_repo_itself ".vim/bundle/neco-ghc"
        neocomplicache = link_repo_itself ".vim/bundle/neocomplcache"

hidden ∷ FilePath → IO a → IO ()
hidden fp x = renameFile fp (fp <.> "old") >> x >> renameFile (fp <.> "old") fp

step ∷ IO Biegunka → IO ()
step link =
  do withBiegunka (\b → merge b <$> link)
     readFile "/home/maksenov/.biegunka.db" >>= print

main ∷ IO ()
main = hidden "/home/maksenov/.biegunka.db" $ step link_one >> step link_two
