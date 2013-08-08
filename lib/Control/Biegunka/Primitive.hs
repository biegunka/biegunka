{-# LANGUAGE DataKinds #-}
-- | Language primitives
--
-- Containts 'Actions' layer primitive and modifiers.
-- 'Sources' layer primitives are found in 'Biegunka.Source.*' modules
--
-- All concrete primitives docs assume you have default settings
module Control.Biegunka.Primitive
  ( -- * Actions layer primitives
    link, register, copy, copyFile, copyDirectory, substitute, patch
  , shell, raw
    -- * Modifiers
  , profile, group
  , sudo, reacting, prerequisiteOf, (<~>)
  ) where

import Data.Monoid (mempty)

import           Control.Lens
import qualified Data.Set as S
import           System.FilePath ((</>))
import           System.FilePath.Lens
import           System.Process (CmdSpec(..))
import           Text.StringTemplate (newSTMP, render, setAttribute)

import Control.Biegunka.Language
import Control.Biegunka.Script


infixr 7 `prerequisiteOf`, <~>


-- | Provides convenient 'Sources' grouping. May be nested
--
-- Information about sources and files related to a particular
-- profile @profile@ could be found in @~\/.biegunka\/profiles\/@.
--
-- Example usage:
--
-- > profile "dotfiles" $ do
-- >   group "mine" $
-- >     git "https://github.com/supki/.dotfiles"
-- >       ...
-- >   group "not-mine" $
-- >     git "https://github.com/dmalikov/dotfiles"
-- >       ...
-- > profile "experimental" $ do
-- >   git "https://github.com/ekmett/lens"
-- >     ...
profile :: String -> Script Sources a -> Script Sources a
profile name inner = do
  p <- Script $ use profileName
  Script $ do
    p' <- profileName <</>= name
    profiles . contains p' .= True
  a <- inner
  Script $ profileName .= p
  return a
{-# INLINE profile #-}

-- | Alias for 'profile'. May be useful for nested grouping
group :: String -> Script Sources a -> Script Sources a
group = profile
{-# INLINE group #-}

-- | Links source to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   register "somewhere"
--
-- Links @~\/git\/source@ to @~\/somewhere@.
register :: FilePath -> Script Actions ()
register dst = actioned (\rfp _ -> Link mempty (rfp </> dst))
{-# INLINE register #-}

-- | Links given file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   link "some-file" "anywhere"
--
-- Links @~\/git\/source\/some-file@ to @~\/anywhere@.
link :: Target p => FilePath -> p -> Script Actions ()
link src dst = actioned (\rfp sfp -> Link (sfp </> src) (constructToFilepath rfp src dst))
{-# INLINE link #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copy :: Target p => FilePath -> p -> Script Actions ()
copy = copy' BothDirectoriesAndFiles
{-# INLINE copy #-}

-- | Copies only single file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyFile :: Target p => FilePath -> p -> Script Actions ()
copyFile = copy' OnlyFiles
{-# INLINE copyFile #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyDirectory :: Target p => FilePath -> p -> Script Actions ()
copyDirectory = copy' OnlyDirectories
{-# INLINE copyDirectory #-}

copy' :: Target p => CopySpec -> FilePath -> p -> Script Actions ()
copy' spec src dst = actioned (\rfp sfp ->
  Copy (sfp </> src) (constructToFilepath rfp src dst) spec)
{-# INLINE copy' #-}

-- | Substitutes templates in @HStringTemplate@ syntax
-- in given file and writes result to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   substitute "some-file.template" "anywhere"
--
-- Copies @~\/git\/source\/some-file.template@ to @~\/anywhere@.
--
-- Substitutes templates in @~\/anywhere@ with values from
-- 'templates' part of 'Controls'
substitute :: Target p => FilePath -> p -> Script Actions ()
substitute src dst = actioned (\rfp sfp ->
  Template (sfp </> src) (constructToFilepath rfp src dst)
    (\b -> render . setAttribute "template" b . newSTMP))
{-# INLINE substitute #-}

-- | Applies the patch given the 'PatchSpec'
--
-- > git "https://example.com/source.git" "git/source" $
-- >   patch "some-patch.patch" "anywhere" (def { reversely = True })
--
-- Applies @~\/git\/source\/some-patch.patch@ to @~\/anywhere@ reversely.
patch :: FilePath -> FilePath -> PatchSpec -> Script Actions ()
patch src dst spec = actioned (\rfp sfp -> Patch (sfp </> src) (rfp </> dst) spec)
{-# INLINE patch #-}


-- | Executes shell command with default shell
--
-- > git "https://example.com/source.git" "git/source" $
-- >   shell "echo hello"
--
-- Prints \"hello\\n\" to stdout
shell :: String -> Script Actions ()
shell command = actioned (\_ sfp -> Command sfp (ShellCommand command))
{-# INLINE shell #-}

-- | Executes raw command
--
-- > git "https://example.com/source.git" "git/source" $
-- >   raw "/bin/echo" ["-n", "hello"]
--
-- Prints \"hello\" to stdout
raw :: FilePath -> [String] -> Script Actions ()
raw command args = actioned (\_ sfp -> Command sfp (RawCommand command args))
{-# INLINE raw #-}

-- | Change effective user id for wrapped commands
sudo :: User -> Script s a -> Script s a
sudo user inner = do
  script (TM (User (Just user)) ())
  a <- inner
  script (TM (User Nothing) ())
  return a
{-# INLINE sudo #-}

-- | Change reaction pattern for wrapped commands
reacting :: React -> Script s a -> Script s a
reacting reaction inner = do
  script (TM (Reacting (Just reaction)) ())
  a <- inner
  script (TM (Reacting Nothing) ())
  return a
{-# INLINE reacting #-}

-- | Execute scripts sequentially
-- Connects two scripts which forces them to run sequentially one after another.
prerequisiteOf :: Script Sources a -> Script Sources b -> Script Sources b
prerequisiteOf a b = do
  s <- Script $ use token
  a
  t <- Script $ use token
  script (TM (Wait (S.fromList [s .. t - 1])) ())
  b
{-# INLINE prerequisiteOf #-}

-- | Infix alias for 'prerequisiteOf'
(<~>) :: Script Sources a -> Script Sources b -> Script Sources b
(<~>) = prerequisiteOf
{-# INLINE (<~>) #-}
