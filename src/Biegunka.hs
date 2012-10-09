{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters
    pretend, execute, verify
    -- * Profile layer
  , profile
    -- * File layer
  , message, registerAt, copy, link, ghc, substitute
  , chmod, chown
    -- * Settings
  , root, sourceRoot, setting, template
    -- * Convenient type aliases
  , ProfileScript, SourceScript, FileScript
  , SimpleProfileScript, SimpleSourceScript, SimpleFileScript
  ) where

import Biegunka.DSL
  ( ProfileScript, SourceScript, FileScript
  , profile
  , message, registerAt, copy, link, ghc, substitute
  , chmod, chown
  )

import Biegunka.Settings (root, sourceRoot, setting, template)

import Biegunka.Interpreter.Pretend (pretend)
import Biegunka.Interpreter.Execute (execute)
import Biegunka.Interpreter.Verify (verify)


-- | Type alias of ProfileScript for those who doesn't care about customs
type SimpleProfileScript a = ProfileScript () () a


-- | Type alias of SourceScript for those who doesn't care about customs
type SimpleSourceScript a = SourceScript () () a


-- | Type alias of FileScript for those who doesn't care about customs
type SimpleFileScript a = FileScript () () a
