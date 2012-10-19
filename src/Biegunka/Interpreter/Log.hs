{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Log (full) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Int (Int64)
import Data.Monoid ((<>), mempty)

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (execWriter, tell)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromString, toLazyText)

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.DSL (Command(..), Action(..), Wrapper(..), mfoldie)


full ∷ Free (Command l ()) () → Biegunka → Biegunka → Text
full s α β = toLazyText $ install s <> uninstall α β


install ∷ Free (Command l ()) () → Builder
install = mfoldie g


g ∷ Command l () (Free (Command l ()) ()) → Builder
g (P name _ _) = "Setup profile " <> string name <> "\n"
g (S u p _ _ _) = indent 2 <> "Setup repository " <> string u <> " at " <> string p <> "\n"
g (S' {}) = mempty
g (F a _) = h a
 where
  h (Message m) = indent 4 <>
    "Message: " <> string m <> "\n"
  h (RegisterAt src dst) = indent 4 <>
    "Link repository " <> string src <> " to " <> string dst <> "\n"
  h (Link src dst) = indent 4 <>
    "Link file " <> string src <> " to " <> string dst <> "\n"
  h (Copy src dst) = indent 4 <>
    "Copy file " <> string src <> " to " <> string dst <> "\n"
  h (Compile cmp src dst) = indent 4 <>
    "Compile with " <> string (show cmp) <> " file " <> string src <> " to " <> string dst <> "\n"
  h (Template src dst _) = indent 4 <>
    "Write " <> string src <> " with substituted templates to " <> string dst <> "\n"
  h (Mode fp mode) = indent 4 <>
    "Set " <> string fp <> " mode to " <> string (show mode) <> "\n"
  h (Ownership fp user group) = indent 4 <>
    "Set " <> string fp <> " owner to " <> string user <> ":" <> string group <> "\n"
g (W a _) = h a
 where
  h (Ignorance _) = mempty
  h (User (Just user)) = "--- * Do stuff from user " <> string user <> " * ---"
  h (User Nothing) = "--- * Do stuff from default user * ---"


indent ∷ Int64 → Builder
indent n = fromLazyText $ T.replicate n " "


uninstall ∷ Biegunka → Biegunka → Builder
uninstall α β = (logNotElems `on` filepaths) α β <> (logNotElems `on` sources) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " <> string x <> "\n"))


string ∷ String → Builder
string = fromString
