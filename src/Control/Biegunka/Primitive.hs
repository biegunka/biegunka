{-# LANGUAGE DataKinds #-}
-- | Language primitives
--
-- Containts 'Actions' layer primitive and modifiers.
-- 'Sources' layer primitives are found in 'Biegunka.Source.*' modules
--
-- All concrete primitives docs assume you have default settings
module Control.Biegunka.Primitive
  ( -- * Actions layer primitives
    link, register, copy, copyFile, copyDirectory, substitute
  , raw
    -- * Modifiers
  , namespace
  , sudo, retries, reacting, prerequisiteOf, (<~>)
  ) where

import           Control.Lens
import           Control.Monad.Reader (local)
import qualified Data.Set as S
import           System.FilePath ((</>))
import           System.Command.QQ (Eval(..))

import Control.Biegunka.Language
import Control.Biegunka.Script
import Control.Biegunka.Script.Token (peek)
import Control.Biegunka.Templates


infixr 7 `prerequisiteOf`, <~>


-- | Namespaces group 'Sources' together; they can be nested.
--
-- Example usage:
--
-- > namespace "dotfiles" $ do
-- >   namespace "mine" $
-- >     git "https://github.com/supki/.dotfiles" ...
-- >       ...
-- >   namespace "not-mine" $
-- >     git "https://github.com/dmalikov/dotfiles" ...
-- >       ...
-- > namespace "experimental" $ do
-- >   git "https://github.com/ekmett/lens"
-- >     ...
namespace :: String -> Script 'Sources a -> Script 'Sources a
namespace segment (Script inner) = Script $
  local (over segments (segment :)) $ do
    xs <- view segments
    namespaces . contains xs .= True
    inner
{-# INLINE namespace #-}

-- | Links source to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   register "somewhere"
--
-- Links @~\/git\/source@ to @~\/somewhere@.
register :: FilePath -> Script 'Actions ()
register dst = actioned (\rfp sfp -> Link sfp (rfp </> dst))
{-# INLINE register #-}

-- | Links given file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   link "some-file" "anywhere"
--
-- Links @~\/git\/source\/some-file@ to @~\/anywhere@.
link :: FilePath -> FilePath -> Script 'Actions ()
link src dst = actioned (\rfp sfp -> Link (sfp </> src) (constructTargetFilePath rfp src dst))
{-# INLINE link #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copy :: FilePath -> FilePath -> Script 'Actions ()
copy = copy' BothDirectoriesAndFiles
{-# INLINE copy #-}

-- | Copies only single file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyFile :: FilePath -> FilePath -> Script 'Actions ()
copyFile = copy' OnlyFiles
{-# INLINE copyFile #-}

-- | Copies file or directory to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
copyDirectory :: FilePath -> FilePath -> Script 'Actions ()
copyDirectory = copy' OnlyDirectories
{-# INLINE copyDirectory #-}

copy' :: CopySpec -> FilePath -> FilePath -> Script 'Actions ()
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
substitute :: FilePath -> FilePath -> Script 'Actions ()
substitute src dst = actioned (\rfp sfp ->
  Template (sfp </> src) (constructTargetFilePath rfp src dst) templating)
{-# INLINE substitute #-}


-- | Monomorphised interface to 'sh' quasiquoter for
-- those who do not like @-XTemplateHaskell@ (or @-XQuasiQuotes@)
--
-- > git "https://example.com/source.git" "git/source" $
-- >   raw "/bin/echo" ["-n", "hello"]
--
-- Prints \"hello\" to stdout
raw :: FilePath -> [String] -> Script 'Actions ()
raw = eval
{-# INLINE raw #-}

-- | Change effective user id for wrapped commands
sudo :: User -> Script s a -> Script s a
sudo user (Script inner) = Script $
  (activeUser ?~ user) `local` inner
{-# INLINE sudo #-}

-- | Change maximum retries count
retries :: Int -> Script s a -> Script s a
retries count (Script inner) = Script $
  set maxRetries (Retries count) `local` inner
{-# INLINE retries #-}

-- | Change reaction pattern when retries are all failed
reacting :: React -> Script s a -> Script s a
reacting reaction (Script inner) = Script $
  (set actionReaction reaction . set sourceReaction reaction) `local` inner
{-# INLINE reacting #-}

-- | Execute scripts sequentially
-- Connects two scripts which forces them to run sequentially one after another.
prerequisiteOf :: Script 'Sources a -> Script 'Sources b -> Script 'Sources b
prerequisiteOf a b = do
  s <- Script peek
  a
  t <- Script peek
  script (TM (Wait (S.fromList [s .. pred t])) ())
  b
{-# INLINE prerequisiteOf #-}

-- | Infix alias for 'prerequisiteOf'
(<~>) :: Script 'Sources a -> Script 'Sources b -> Script 'Sources b
(<~>) = prerequisiteOf
{-# INLINE (<~>) #-}
