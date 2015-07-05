-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git
  ( -- * Source layer
    git', git, git_
    -- * Types
  , Git
    -- * Modifiers
  , actions, branch, failIfAhead
    -- * Type synonyms
  , URI
  ) where

import           Control.Biegunka.Source.Git.Internal
