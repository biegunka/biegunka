{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.ConstructMap (construct) where

import Data.Monoid (Monoid(..))

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           System.FilePath ((</>))

import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Repository (Repository(..))
import Biegunka.DSL.Files (Files(..))


construct ∷ FilePath → Free (Profile a) b → Map String (Map FilePath (Set FilePath))
construct home (Free (Profile name script next)) =
  M.insertWith' mappend name (profile home script) (construct home next)
construct _ (Pure _) = mempty


profile ∷ FilePath → Free (Repository a) b → Map FilePath (Set FilePath)
profile home (Free (Git _ path script next)) = M.singleton path (repo home script) `mappend` profile home next
profile _ (Pure _) = mempty


repo ∷ FilePath → Free Files a → Set FilePath
repo home script = execWriter (runScript script)
 where
  runScript ∷ Free Files a → Writer (Set FilePath) a
  runScript (Free (Message _ x)) = runScript x
  runScript (Free (RegisterAt dst x)) = tell (S.singleton (home </> dst)) >> runScript x
  runScript (Free (Link _ dst x)) = tell (S.singleton (home </> dst)) >> runScript x
  runScript (Free (Copy _ dst x)) = tell (S.singleton (home </> dst)) >> runScript x
  runScript (Free (Compile _ _ _ x)) = runScript x
  runScript (Pure x) = return x
