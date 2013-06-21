-- | Biegunka - configuration development library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root, appData, colors
    -- * Interpreters
  , pretend, pause, confirm, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Templates(..), templates
  , retries
    -- * Profile layer primitives
  , profile
    -- * Source layer primitives
  , Source(..)
    -- * Action layer primitives
  , registerAt, copy, link, substitute, shell
    -- * All layers primitives
  , sudo, reacting, chain, (<~>)
    -- * Types
  , Script, Scope(..)
    -- * TH
  , makeOptionsParser
  ) where

import Biegunka.Control (biegunka, Controls, root, appData, colors, pause, confirm)
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
import Biegunka.TH (makeOptionsParser)
import Biegunka.Verify (verify)
import Biegunka.Source (Source(..))
