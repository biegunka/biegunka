{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Pretend (pretend) where

import Data.Monoid ((<>))

import Control.Monad.Free (Free(..))
import System.FilePath ((</>))

import Biegunka.Repository (Repository(..))
import Biegunka.Script (Script(..))


pretend ∷ Free (Repository a) b → String
pretend (Free (Git url path script next)) =
  "Setup repository " <> url <> " at " <> path <> "\n" <> repo script <> "\n" <> pretend next
 where
  repo (Free (Message m x)) = "Message: " <> show m <> "\n" <> repo x
  repo (Free (LinkRepo dst x)) = "Link repository " <> path <> " to ~/" <> dst <> "\n" <> repo x
  repo (Free (LinkRepoFile src dst x)) = "Link file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> repo x
  repo (Free (CopyRepoFile src dst x)) = "Copy file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> repo x
  repo (Free (Compile cmp src dst x)) =
    "Compile with " <> show cmp <> " file " <> (path </> src) <> " to ~/" <> dst <> "\n" <> repo x
  repo (Pure _) = ""
pretend (Pure _) = ""
