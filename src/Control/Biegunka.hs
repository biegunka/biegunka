-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, defaultSettings, runRoot, biegunkaRoot
  , Templates(..), templates
    -- * Interpreters
  , Interpreter
  , pause, confirm, changes, run, check
    -- * Types
  , Script, Scope(..)
    -- * Actions layer primitives
  , link, register, copy, unE, substitute, raw
    -- * Script environment
  , sourceRoot
    -- * Modifiers
  , namespace
  , sudo, User(..), username, uid, retries, reacting, React(..), prerequisiteOf, (<~>)
    -- * Auxiliary
  , into
    -- * Commandline options parser autogeneration
  , runner_, runnerOf, Environments(..), Generic
    -- * Quasiquoters
  , multiline, sh, shell
    -- * Settings
    -- ** Mode
  , mode, Mode(..)
    -- * Little helpers
  , (~>)
  , pass
  ) where

import GHC.Generics (Generic)
import System.Directory.Layout (username, uid)

import Control.Biegunka.Biegunka (biegunka)
import Control.Biegunka.Settings
  ( Settings, defaultSettings, biegunkaRoot
  , Templates(..), templates
  , mode, Mode(..)
  )
import Control.Biegunka.Execute (run)
import Control.Biegunka.Interpreter (Interpreter, pause, confirm, changes)
import Control.Biegunka.Language (Scope(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (runRoot, sourceRoot, Script, User(..), React(..), into)
import Control.Biegunka.QQ (multiline, sh, shell)
import Control.Biegunka.Options (Environments(..), runnerOf, runner_)
import Control.Biegunka.Check (check)


infix 4 ~>
-- | An alias for '(,)' for better looking pairing
(~>) :: a -> b -> (a, b)
(~>) = (,)

-- | Do nothing.
pass :: Applicative m => m ()
pass = pure ()
