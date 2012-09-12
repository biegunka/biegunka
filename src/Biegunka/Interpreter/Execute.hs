{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Data.Function (on)
import Data.Monoid (mempty)

import           Control.Lens ((^.))
import qualified Data.Text.Lazy.IO as T
import           Control.Monad.Free (Free(..))
import           Data.Default (Default)
import           System.Directory
  ( copyFile, createDirectoryIfMissing
  , getHomeDirectory, removeDirectoryRecursive, removeFile
  )
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (runProcess, waitForProcess)

import           Biegunka.DB
import           Biegunka.DSL
  ( ProfileScript
  , Command(..)
  , update, script
  , Profile, Source, Files
  , Compiler(..), foldie)
import qualified Biegunka.Interpreter.Common.Map as Map
import           Biegunka.Interpreter.Common.State


-- | Execute Interpreter
--
-- Execute script. Copy and links files, compiles stuff. You get the idea
--
-- @
-- main ∷ IO ()
-- main = execute $ do
--   profile ...
--   profile ...
-- @
execute ∷ (Default s, Default t) ⇒ ProfileScript s t () → IO ()
execute s = do
  home ← getHomeDirectory
  let s' = infect home s
  α ← load
  when (α == mempty) $
    putStrLn "Warning: Biegunka is empty"
  profile s'
  let β = Map.construct s'
  removeOrphanFiles α β
  removeOrphanRepos α β
  save β
 where
  removeOrphanFiles = removeOrphan removeFile filepaths
  removeOrphanRepos = removeOrphan removeDirectoryRecursive sources

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) $ f x


profile ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) () → IO ()
profile = foldie (>>) (return ()) f
 where
  f ∷ Command Profile (Free (Command Source (Free (Command Files ()) ())) ()) a → IO ()
  f (Profile _ s _) = source s


source ∷ Free (Command Source (Free (Command Files ()) ())) () → IO ()
source = foldie (>>) (return ()) f
 where
  f s = (s^.update) >> files (s^.script)


files ∷ Free (Command Files ()) a → IO ()
files = foldie (>>) (return ()) f
 where
  f ∷ Command Files () a → IO ()
  f (Message m _) = putStrLn m
  f (RegisterAt src dst _) = overWriteWith createSymbolicLink src dst
  f (Link src dst _) = overWriteWith createSymbolicLink src dst
  f (Copy src dst _) = overWriteWith copyFile src dst
  f (Compile cmp src dst _) = compileWith cmp src dst
  f (Template src dst substitute _) = substitute <$> readFile src >>= T.writeFile dst


overWriteWith ∷ (FilePath → FilePath → IO ()) → FilePath → FilePath → IO ()
overWriteWith f src dst =
  do createDirectoryIfMissing True $ dropFileName dst
     try $ removeFile dst ∷ IO (Either SomeException ())
     f src dst


compileWith ∷ Compiler → FilePath → FilePath → IO ()
compileWith GHC src dst =
  do waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName src
