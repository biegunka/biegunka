-- | Biegunka - configuration development library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root, appData, colors
    -- * Interpreters
  , pause, confirm, dryRun, run, check
  , pretend, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Templates(..), templates
  , retries
    -- * Profiles layer primitives
  , profile
    -- * Sources layer primitives
  , Source(..)
    -- * Actions layer primitives
  , link, register, copy, substitute, shell
    -- * All layers primitives
  , sudo, reacting, chain, (<~>)
    -- * Types
  , Script, Scope(..)
    -- * TH
  , makeOptionsParser
  ) where

import Biegunka.Control (biegunka, Controls, root, appData, colors, pause, confirm)
import Biegunka.Execute (execute, run)
import Biegunka.Execute.Control
  ( EE
  , Priviledges(..), priviledges
  , react
  , Templates(..), templates
  , retries
  )
import Biegunka.Language (Scope(..), React(..))
import Biegunka.Pretend (dryRun, pretend)
import Biegunka.Primitive
import Biegunka.Script (Script)
import Biegunka.TH (makeOptionsParser)
import Biegunka.Verify (check, verify)
import Biegunka.Source (Source(..))
