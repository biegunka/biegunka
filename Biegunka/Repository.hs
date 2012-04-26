{-# LANGUAGE UnicodeSyntax #-}
module Biegunka.Repository
  ( Repository(..)
  ) where

class Repository a where
  clone ∷ a → IO Bool
  update ∷ a → IO Bool
  path ∷ a → String
