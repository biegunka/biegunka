{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.Repository.Git
  ( git
  ) where

import Biegunka.Repository

type UrlPath = String
data Git = Git { url ∷ String, dir ∷ FilePath }

git ∷ UrlPath → FilePath → Git
git = Git

instance Repository Git where
  clone = gitClone
  update = gitPull
  hash = gitHash

gitClone ∷ Git → IO Git
gitClone = undefined

gitPull ∷ Git → IO Git
gitPull = undefined

gitHash ∷ Git → String
gitHash = url
