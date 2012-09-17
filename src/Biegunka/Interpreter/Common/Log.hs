{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Common.Log (full) where

import Control.Monad (forM_, unless)
import Data.Function (on)
import Data.Int (Int64)
import Data.Monoid ((<>))

import           Control.Lens ((^.))
import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (execWriter, tell)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder (Builder, fromLazyText, fromString, toLazyText)

import Biegunka.DB (Biegunka, filepaths, sources)
import Biegunka.DSL
  ( Layer(..), Command(..)
  , from, to, script
  , mfoldie
  )


full ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
     → Biegunka
     → Biegunka
     → Text
full s α β = toLazyText $ install s <> uninstall α β


install ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → Builder
install = profile


profile ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → Builder
profile = mfoldie f
 where
  f ∷ Command Profile (Free (Command Source (Free (Command Files ()) ())) ()) a → Builder
  f (P name s _) = "Setup profile " <> string name <> "\n" <> source s


source ∷ Free (Command Source (Free (Command Files ()) ())) () → Builder
source = mfoldie f
 where
  f s = indent 2 <>
    "Setup repository " <> string (s^.from) <> " at " <> string (s^.to) <> "\n" <> files (s^.script)


files ∷ Free (Command Files ()) () → Builder
files = mfoldie f
 where
  f ∷ Command Files () a → Builder
  f (Message m _) = indent 4 <>
    "Message: " <> string m <> "\n"
  f (RegisterAt src dst _) = indent 4 <>
    "Link repository " <> string src <> " to " <> string dst <> "\n"
  f (Link src dst _) = indent 4 <>
    "Link file " <> string src <> " to " <> string dst <> "\n"
  f (Copy src dst _) = indent 4 <>
    "Copy file " <> string src <> " to " <> string dst <> "\n"
  f (Compile cmp src dst _) = indent 4 <>
    "Compile with " <> string (show cmp) <> " file " <> string src <> " to " <> string dst <> "\n"
  f (Template src dst _ _) = indent 4 <>
    "Write " <> string src <> " with substituted templates to " <> string dst <> "\n"
  f (Mode fp m _) = indent 4 <>
    "Set " <> string fp <> " mode to " <> string (show m) <> "\n"
  f (Ownership fp u g _) = indent 4 <>
    "Set " <> string fp <> " owner to " <> string u <> ":" <> string g <> "\n"


indent ∷ Int64 → Builder
indent n = fromLazyText $ T.replicate n " "


uninstall ∷ Biegunka → Biegunka → Builder
uninstall α β = (logNotElems `on` filepaths) α β <> (logNotElems `on` sources) α β
 where
  logNotElems xs ys = execWriter (forM_ xs $ \x → unless (x `elem` ys) (tell $ "Delete " <> string x <> "\n"))


string ∷ String → Builder
string = fromString
