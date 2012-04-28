module Biegunka.Script where

import Biegunka.Core

instance ScriptI Script where
  message = message_
  link_repo_itself = link_repo_itself_
  link_repo_file = link_repo_file_

link_repo_itself_ ∷ FilePath → Script ()
link_repo_itself_ = undefined

link_repo_file_ ∷ FilePath → FilePath → Script ()
link_repo_file_ = undefined

message_ ∷ String → Script ()
message_ = undefined
