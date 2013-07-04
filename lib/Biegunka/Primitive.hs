{-# LANGUAGE DataKinds #-}
-- | Language primitives
module Biegunka.Primitive
  ( -- * Profile layer primitives
    profile
    -- * Actions layer primitives
  , link, register, copy, substitute
  , shell, raw
    -- * Modifiers
  , sudo, reacting, chain, (<~>)
  ) where

import Data.Monoid (mempty)

import Control.Lens
import Control.Monad.State
import Control.Monad.Free (liftF)
import System.FilePath ((</>))
import System.Process (CmdSpec(..))
import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Language
import Biegunka.Script


infixr 7 `chain`, <~>


-- | Provides convenient 'Sources' grouping. Does not nest
--
-- Information about sources and files related to a particular
-- profile @profile@ could be found in @~\/.biegunka\/profile@.
-- (Assuming default settings.)
--
-- Example usage:
--
-- > profile "dotfiles" $ do
-- >   git "https://github.com/supki/.dotfiles" ...
-- >   git "https://github.com/dmalikov/dotfiles"
-- > profile "experimental" $ do
-- >   git "https://github.com/ekmett/lens" ...
profile :: String -> Script Sources () -> Script Profiles ()
profile name inner = Script $ do
  tok <- use token
  ast <- annotate inner
  lift . liftF $ TP (AP { apToken = tok }) (Profile name) ast ()
  token += 1
{-# INLINE profile #-}

-- | Links source to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   register "somewhere"
--
-- Links @~\/git\/source@ to @~\/somewhere@.
-- (Assuming default settings.)
register :: FilePath -> Script Actions ()
register dst = actioned (\rfp _ -> Link mempty (rfp </> dst))
{-# INLINE register #-}

-- | Links given file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   link "some-file" "anywhere"
--
-- Links @~\/git\/source\/some-file@ to @~\/anywhere@.
-- (Assuming default settings.)
link :: FilePath -> FilePath -> Script Actions ()
link src dst = actioned (\rfp sfp -> Link (sfp </> src) (constructDestinationFilepath rfp src dst))
{-# INLINE link #-}

-- | Copies given file to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   copy "some-file" "anywhere"
--
-- Copies @~\/git\/source\/some-file@ to @~\/anywhere@.
-- (Assuming default settings.)
copy :: FilePath -> FilePath -> Script Actions ()
copy src dst = actioned (\rfp sfp -> Copy (sfp </> src) (constructDestinationFilepath rfp src dst))
{-# INLINE copy #-}

-- | Substitutes templates in @HStringTemplate@ syntax
-- in given file and writes result to specified filepath
--
-- > git "https://example.com/source.git" "git/source" $
-- >   substitute "some-file.template" "anywhere"
--
-- Copies @~\/git\/source\/some-file.template@ to @~\/anywhere@.
-- (Assuming default settings.)
--
-- Substitutes templates in @~\/anywhere@ with values from
-- 'templates' part of 'Controls'
substitute :: FilePath -> FilePath -> Script Actions ()
substitute src dst = actioned (\rfp sfp ->
  Template (sfp </> src) (constructDestinationFilepath rfp src dst)
    (\b -> render . setAttribute "template" b . newSTMP))
{-# INLINE substitute #-}


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
sudo :: String -> Script s () -> Script s ()
sudo username inner = do
  script (TM (User (Just username)) ())
  inner
  script (TM (User Nothing) ())
{-# INLINE sudo #-}

-- | Change reaction pattern for wrapped commands
reacting :: React -> Script s () -> Script s ()
reacting reaction inner = do
  script (TM (Reacting (Just reaction)) ())
  inner
  script (TM (Reacting Nothing) ())
{-# INLINE reacting #-}

-- | Chain tasks sequentially
-- Connects two tasks which forces them to run sequentially one after another.
chain :: Script s () -> Script s () -> Script s ()
chain a b = Script $ do
  s <- rewind token (annotate a >>= lift)
  t <- rewind token (annotate b >>= lift)
  token .= max s t

-- | Infix alias for 'chain'
(<~>) :: Script s () -> Script s () -> Script s ()
(<~>) = chain
{-# INLINE (<~>) #-}
