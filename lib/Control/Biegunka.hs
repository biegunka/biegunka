-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, root, appData, colors
  , Templates(..), templates
    -- * Interpreters
  , Interpreter
  , pause, confirm, dryRun, run, check
    -- * Types
  , Script, Scope(..), Target
    -- * Sources layer primitives
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, copyFile, copyDirectory, substitute, patch, PatchSpec(..)
  , shell, raw
    -- * Modifiers
  , profile, group, role
  , sudo, User(..), retries, reacting, React(..), prerequisiteOf, (<~>)
    -- * Auxiliary
  , Into, into
    -- * TH
  , biegunkaOptions
    -- * Quasiquoters
  , sh
    -- ** Colors
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
  ) where

import Control.Biegunka.Biegunka (Interpreter, biegunka, pause, confirm)
import Control.Biegunka.Settings
  ( Settings, root, appData, colors
  , ColorScheme(..)
  , actionColor, sourceColor, srcColor, dstColor, errorColor, retryColor, noColors
  , Templates(..), templates
  )
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(..), PatchSpec(..))
import Control.Biegunka.Primitive
import Control.Biegunka.QQ (sh)
import Control.Biegunka.Script (Script, User(..), React(..), Target, Into, into)
import Control.Biegunka.TH (biegunkaOptions)
import Control.Biegunka.Check (check)
import Control.Biegunka.Source (Sourceable(..))
