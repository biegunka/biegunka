{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
  , link, register, copy, copyFile, copyDirectory, substitute, patch, PatchSpec(..), raw
    -- * Script environment
  , root, source
    -- * Modifiers
  , profile, group, role
  , sudo, User(..), retries, reacting, React(..), prerequisiteOf, (<~>)
    -- * Auxiliary
  , into
    -- * TH
  , biegunkaOptions
    -- * Quasiquoters
  , sh, shell
    -- * Settings
    -- ** Colors
  , ColorScheme(..)
  , actionColor, sourceColor
  , srcColor, dstColor
  , errorColor, retryColor
  , noColors
    -- ** Mode
  , mode, Mode(..)
  ) where

import Control.Biegunka.Biegunka (Interpreter, biegunka, pause, confirm)
import Control.Biegunka.Settings
  ( Settings, appData, colors
  , ColorScheme(..)
  , actionColor, sourceColor, srcColor, dstColor, errorColor, retryColor, noColors
  , Templates(..), templates
  , mode, Mode(..)
  )
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(..), PatchSpec(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (root, source, Script, User(..), React(..), into)
import Control.Biegunka.TH (biegunkaOptions, sh, shell)
import Control.Biegunka.Check (check)
import Control.Biegunka.Source (Sourceable(..))
