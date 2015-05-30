-- | Biegunka - configuration development library
module Control.Biegunka
  ( -- * Interpreters control
    biegunka, Settings, defaultSettings, runRoot, biegunkaRoot
  , Templates(..), templates
    -- * Interpreters
  , Interpreter
  , pause, confirm, dryRun, run, check
    -- * Types
  , Script, Scope(..)
    -- * Sources layer primitives
  , Sourceable(..)
    -- * Actions layer primitives
  , link, register, copy, substitute, raw
    -- * Script environment
  , sourceRoot
    -- * Modifiers
  , namespace
  , sudo, User(..), username, uid, retries, reacting, React(..), prerequisiteOf, (<~>)
    -- * Auxiliary
  , into
    -- * Commandline options parser autogeneration
  , runner_, runnerOf, Environments, Generic, Proxy(Proxy)
    -- * Quasiquoters
  , multiline, sh, shell
    -- * Settings
    -- ** Mode
  , mode, Mode(..)
    -- * Little helpers
  , (~>)
  ) where

import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)
import System.Directory.Layout (username, uid)

import Control.Biegunka.Biegunka (Interpreter, biegunka, pause, confirm)
import Control.Biegunka.Settings
  ( Settings, defaultSettings, biegunkaRoot
  , Templates(..), templates
  , mode, Mode(..)
  )
import Control.Biegunka.Execute (run, dryRun)
import Control.Biegunka.Language (Scope(..))
import Control.Biegunka.Primitive
import Control.Biegunka.Script (runRoot, sourceRoot, Script, User(..), React(..), into)
import Control.Biegunka.QQ (multiline, sh, shell)
import Control.Biegunka.Options (Environments, runnerOf, runner_)
import Control.Biegunka.Check (check)
import Control.Biegunka.Source (Sourceable(..))


infix 4 ~>
-- | An alias for '(,)' for better looking pairing
(~>) :: a -> b -> (a, b)
(~>) = (,)
