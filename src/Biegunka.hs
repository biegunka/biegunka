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
  , message, registerAt, copy, link, substitute
  , shell
  , sudo, ignorant
    -- * Convenient type aliases
  , Script, Layer(..)
  ) where

import Biegunka.DSL
  ( Script, Layer(..)
  , profile
  , message, registerAt, copy, link, substitute
  , shell
  , sudo, ignorant
  )
import Biegunka.Pretend (pretend)
import Biegunka.Execute
  ( execute, executeWith
  , OnFail(..)
  , dropPriviledges, react, templates, defaultExecution
  )
import Biegunka.Verify (verify)
