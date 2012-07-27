-- | Biegunka.Core is module defining interfaces for repositories and script engines.
-- Also it provides a "glue" combining repository layer and script one.
module Biegunka.Core
  ( dryRun, execute
  ) where

import Biegunka.Interpreter (dryRun, execute)
