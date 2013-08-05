-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, root, appData, colors
  , Templates(..), templates
    -- * Interpreters
  , Interpreter
  , pause, confirm, dryRun, run, check
    -- * Types
  , Script, Scope(..)
    -- * Sources layer primitives
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, copyFile, copyDirectory, substitute, patch, PatchSpec(..)
  , shell, raw
    -- * Modifiers
  , profile, group
  , sudo, User(..), retries, reacting, React(..), prerequisiteOf, (<~>)
    -- * TH
  , biegunkaOptions
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
  , Templates(..), templates
  )
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(..), PatchSpec(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (Script, User(..), React(..))
import Control.Biegunka.TH (biegunkaOptions)
import Control.Biegunka.Verify (check)
import Control.Biegunka.Source (Sourceable(..))
