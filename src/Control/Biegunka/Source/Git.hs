-- | Git repositories as ''Sources'.
module Control.Biegunka.Source.Git
  ( Git
  , git
  , git_
  , Url
  , Config
  , url
  , path
  , branch
  , failIfAhead
  ) where

import Control.Biegunka.Source.Git.Internal
