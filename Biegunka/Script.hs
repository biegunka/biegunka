module Biegunka.Script where

import Biegunka

data Host = Root | Home

repoTo ∷ Host → FilePath → Script ()
repoTo = undefined

fromRepoTo ∷ Host → (FilePath, FilePath) → Script ()
fromRepoTo = undefined

message ∷ String → Script ()
message = undefined
