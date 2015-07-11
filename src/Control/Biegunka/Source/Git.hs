-- | Git repositories as ''Sources'.
module Control.Biegunka.Source.Git
  ( Git
  , git
  , Url
  , Config
  , NoUrl
  , NoPath
  , url
  , path
  , branch
  , failIfAhead
  ) where

import Control.Biegunka.Source.Git.Internal
