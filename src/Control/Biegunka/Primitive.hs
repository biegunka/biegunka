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
  , raw
    -- * Modifiers
  , profile, group, role
  , sudo, retries, reacting, prerequisiteOf, (<~>)
  ) where

import Data.Monoid (mempty)

import           Control.Lens
import           Control.Monad.Reader (local)
import qualified Data.Set as S
import           System.FilePath ((</>))
import           System.FilePath.Lens
import           System.Command.QQ (Eval(..))

import Control.Biegunka.Language
import Control.Biegunka.Script
import Control.Biegunka.Script.Token (peek)
import Control.Biegunka.Templates


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
profile name (Script inner) = Script $
  local (profileName </>~ name) $ do
    p' <- view profileName
    profiles . contains p' .= True
    inner
{-# INLINE profile #-}

-- | Alias for 'profile'. May be useful for nested grouping
group :: String -> Script Sources a -> Script Sources a
group = profile
{-# INLINE group #-}

-- | Alias for 'profile'. Everyone uses roles for something
role :: String -> Script Sources a -> Script Sources a
role = profile
{-# INLINE role #-}

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
link :: FilePath -> FilePath -> Script Actions ()
link src dst = actioned (\rfp sfp -> Link (sfp </> src) (constructTargetFilePath rfp src dst))
{-# INLINE link #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copy :: FilePath -> FilePath -> Script Actions ()
copy = copy' BothDirectoriesAndFiles
{-# INLINE copy #-}

-- | Copies only single file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyFile :: FilePath -> FilePath -> Script Actions ()
copyFile = copy' OnlyFiles
{-# INLINE copyFile #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyDirectory :: FilePath -> FilePath -> Script Actions ()
copyDirectory = copy' OnlyDirectories
{-# INLINE copyDirectory #-}

copy' :: CopySpec -> FilePath -> FilePath -> Script Actions ()
copy' spec src dst = actioned (\rfp sfp ->
  Copy (sfp </> src) (constructTargetFilePath rfp src dst) spec)
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
substitute :: FilePath -> FilePath -> Script Actions ()
substitute src dst = actioned (\rfp sfp ->
  Template (sfp </> src) (constructTargetFilePath rfp src dst) templating)
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


-- | Monomorphised interface to 'sh' quasiquoter for
-- those who do not like @-XTemplateHaskell@ (or @-XQuasiQuotes@)
--
-- > git "https://example.com/source.git" "git/source" $
-- >   raw "/bin/echo" ["-n", "hello"]
--
-- Prints \"hello\" to stdout
raw :: FilePath -> [String] -> Script Actions ()
raw = eval
{-# INLINE raw #-}

-- | Change effective user id for wrapped commands
sudo :: User u -> Script s a -> Script s a
sudo user (Script inner) = Script $
  (activeUser ?~ UserW user) `local` inner
{-# INLINE sudo #-}

-- | Change maximum retries count
retries :: Int -> Script s a -> Script s a
retries count (Script inner) = Script $
  set maxRetries (Retry count) `local` inner
{-# INLINE retries #-}

-- | Change reaction pattern when retries are all failed
reacting :: React -> Script s a -> Script s a
reacting reaction (Script inner) = Script $
  (set actionReaction reaction . set sourceReaction reaction) `local` inner
{-# INLINE reacting #-}

-- | Execute scripts sequentially
-- Connects two scripts which forces them to run sequentially one after another.
prerequisiteOf :: Script Sources a -> Script Sources b -> Script Sources b
prerequisiteOf a b = do
  s <- Script peek
  a
  t <- Script peek
  script (TM (Wait (S.fromList [s .. pred t])) ())
  b
{-# INLINE prerequisiteOf #-}

-- | Infix alias for 'prerequisiteOf'
(<~>) :: Script Sources a -> Script Sources b -> Script Sources b
(<~>) = prerequisiteOf
{-# INLINE (<~>) #-}
