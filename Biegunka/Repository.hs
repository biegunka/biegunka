{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.Repository
  ( Repository(..)
  ) where

class Repository a where
  clone ∷ a → IO a
  update ∷ a → IO a
  hash ∷ a → String
