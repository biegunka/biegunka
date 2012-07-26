-- | Biegunka.Core is module defining interfaces for repositories and script engines.
-- Also it provides a "glue" combining repository layer and script one.
module Biegunka.Core
  ( (-->?), (-->)
  , Biegunka, Repository(..)
  ) where

import Data.Set (empty)

import Biegunka.DB (Biegunka, create)
import Biegunka.Interpreter (dryRun, execute)
import Biegunka.Script (Free, Script)


-- | Repository interface.
-- It should support these operations:
class Repository ρ where
  -- | Get (absolute) path to the repository
  path ∷ ρ → String


-- | Execute script dry run. Collect resulting string, print it, return empty Biegunka
(-->?) ∷ Repository ρ ⇒ IO ρ → Free Script a → IO Biegunka
mr -->? script = mr >>= \r → putStrLn (dryRun script (path r)) >> return (create (path r) empty)


-- | Execute script. Return resulting Biegunka
(-->) ∷ Repository ρ ⇒ IO ρ → Free Script a → IO Biegunka
mr --> script = mr >>= \r → execute script (path r)
