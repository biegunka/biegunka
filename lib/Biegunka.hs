-- | Biegunka - configuration development library
module Biegunka
  ( -- * Interpreters control
    biegunka, Settings, root, appData, colors
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
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
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, substitute
  , shell, raw
    -- * Modifiers
  , sudo, reacting, chain, (<~>)
    -- * Types
  , Script, Scope(..)
    -- * TH
  , makeOptionsParser
  ) where

import Biegunka.Control
  ( biegunka, Settings, root, appData, colors, pause, confirm
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
  )
import Biegunka.Execute (run, execute, dryRun, pretend)
import Biegunka.Execute.Control
  ( EE
  , Priviledges(..), priviledges
  , react
  , Templates(..), templates
  , retries
  )
import Biegunka.Language (Scope(..), React(..))
import Biegunka.Primitive
import Biegunka.Script (Script)
import Biegunka.TH (makeOptionsParser)
import Biegunka.Verify (check, verify)
import Biegunka.Source (Sourceable(..))
