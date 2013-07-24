-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, root, appData, colors
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
    -- * Interpreters
  , Interpreter
  , pause, confirm, dryRun, run, check
  , pretend, execute, verify
    -- * Execution environment hooks
  , Run
  , React(..), react
  , Templates(..), templates
  , retries
    -- * Sources layer primitives
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, copyFile, copyDirectory, substitute, patch, PatchSpec(..)
  , shell, raw
    -- * Modifiers
  , profile, group
  , sudo, User(..), reacting, prerequisiteOf, (<~>)
    -- * Types
  , Script, Scope(..)
    -- * TH
  , makeOptionsParser
  ) where

import Control.Biegunka.Settings
  ( biegunka, Settings, root, appData, colors
  , Interpreter, pause, confirm
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
  )
import Control.Biegunka.Execute (run, execute, dryRun, pretend)
import Control.Biegunka.Execute.Settings
  ( Run
  , react
  , Templates(..), templates
  , retries
  )
import Control.Biegunka.Language (Scope(..), React(..), User(..), PatchSpec(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (Script)
import Control.Biegunka.TH (makeOptionsParser)
import Control.Biegunka.Verify (check, verify)
import Control.Biegunka.Source (Sourceable(..))
