module Paths_biegunka
  ( getDataFileName
  , version
  ) where

import Data.Version (Version(..))


getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

version :: Version
version = Version { versionBranch = [1, 0], versionTags = [] }
