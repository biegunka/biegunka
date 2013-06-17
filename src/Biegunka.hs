-- | Biegunka - configuration development library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root, appData, colors
    -- * Interpreters
  , pretend, pause, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Templates(..), templates
  , retries
    -- * Profile layer primitives
  , profile
    -- * Action layer primitives
  , registerAt, copy, link, substitute, shell
    -- * All layers primitives
  , sudo, reacting, chain, (<~>)
    -- * Types
  , Script, Scope(..)
  ) where

import Biegunka.Control (biegunka, Controls, root, appData, colors, pause)
import Biegunka.Execute (execute)
import Biegunka.Execute.Control
  ( EE
  , Priviledges(..), priviledges
  , react
  , Templates(..), templates
  , retries
  )
import Biegunka.Language (Scope(..), React(..))
import Biegunka.Pretend (pretend)
import Biegunka.Primitive
import Biegunka.Script (Script)
import Biegunka.Verify (verify)
