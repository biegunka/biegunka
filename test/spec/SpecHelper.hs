module SpecHelper
  ( withBiegunkaTempDirectory
  ) where

import qualified System.IO.Temp as IO


withBiegunkaTempDirectory :: (FilePath -> IO a) -> IO a
withBiegunkaTempDirectory = IO.withSystemTempDirectory "biegunka"
