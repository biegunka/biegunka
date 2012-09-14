{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters
    pretend, execute, verify
    -- * Profile layer
  , profile
    -- * File layer
  , message, registerAt, copy, link, compile, Compiler(..), substitute
    -- * Settings
  , root, sourceRoot, custom
    -- * Convenient type aliases
  , ProfileScript, SourceScript, FileScript
  ) where

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , profile
  , message, registerAt, copy, link, compile, Compiler(..), substitute
  )

import Biegunka.Settings (root, sourceRoot, custom)

import Biegunka.Interpreter.Pretend (pretend)
import Biegunka.Interpreter.Execute (execute)
import Biegunka.Interpreter.Verify (verify)
