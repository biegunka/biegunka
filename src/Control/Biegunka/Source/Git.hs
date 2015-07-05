-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git
  ( git
  , Git
  , Repository
  , Config
  , url
  , path
  , branch
  , failIfAhead
  ) where

import Control.Biegunka.Source.Git.Internal
