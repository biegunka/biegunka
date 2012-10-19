{-# OPTIONS_HADDOCK prune #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters
    pretend, execute, executeWith, verify
    -- * Interpreters related
  , OnFail(..), dropPriviledges, react, templates, defaultExecution
    -- * Profile layer
  , profile
    -- * File layer
  , message, registerAt, copy, link, ghc, substitute
  , chmod, chown
  , sudo, ignorant
    -- * Convenient type aliases
  , Script, Layer(..)
  ) where

import Biegunka.DSL
  ( Script, Layer(..)
  , profile
  , message, registerAt, copy, link, ghc, substitute
  , chmod, chown
  , sudo, ignorant
  )
import Biegunka.Interpreter.Pretend (pretend)
import Biegunka.Interpreter.Execute
  ( execute, executeWith
  , OnFail(..)
  , dropPriviledges, react, templates, defaultExecution
  )
import Biegunka.Interpreter.Verify (verify)
