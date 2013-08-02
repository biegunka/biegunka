-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, root, appData, colors
    -- * Interpreters
  , Interpreter
  , pause, confirm, dryRun, run, check
  , pretend, execute, verify
    -- * Types
  , Script, Scope(..)
    -- * Sources layer primitives
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, copyFile, copyDirectory, substitute, patch, PatchSpec(..)
  , shell, raw
    -- * Modifiers
  , profile, group
  , sudo, User(..), reacting, React(..), prerequisiteOf, (<~>)
    -- * Execution environment hooks
  , Run
  , Templates(..), templates
  , retries
    -- * TH
  , makeOptionsParser
    -- ** Colors
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
  ) where

import Control.Biegunka.Settings
  ( biegunka, Settings, root, appData, colors
  , Interpreter, pause, confirm
  , ColorScheme(..)
  , actionColor, sourceColor, srcColor, dstColor, errorColor, retryColor, noColors
  )
import Control.Biegunka.Execute (run, execute, dryRun, pretend)
import Control.Biegunka.Execute.Settings (Run, Templates(..), templates, retries)
import Control.Biegunka.Language (Scope(..), React(..), PatchSpec(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (Script, User(..))
import Control.Biegunka.TH (makeOptionsParser)
import Control.Biegunka.Verify (check, verify)
import Control.Biegunka.Source (Sourceable(..))
