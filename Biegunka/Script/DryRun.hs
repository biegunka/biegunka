module Biegunka.Script.DryRun where

import Biegunka

data Host = Root | Home

repoTo ∷ Host → FilePath → Biegunka ()
repoTo = undefined

fromRepoTo ∷ Host → (FilePath, FilePath) → Biegunka ()
fromRepoTo = undefined
