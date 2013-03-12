{-# LANGUAGE DataKinds #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root, appData
    -- * Interpreters
  , pretend, pause, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Volubility(..), volubility
  , Templates(..), templates
  , retries, Order(..), order
    -- * All layers
  , sudo, reacting, task
    -- * Profile layer
  , profile
    -- * File layer
  , registerAt, copy, link, substitute, shell
    -- * Convenient type aliases
  , Script, Layer(..)
  ) where

import Data.Monoid (mempty)

import Control.Monad.Free (Free(..), liftF)
import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Control (biegunka, Controls, root, appData, pause)
import Biegunka.Language.External (Script, Layer(..), EL(..), Action(..), Wrapper(..), React(..))
import Biegunka.Pretend (pretend)
import Biegunka.Execute (execute)
import Biegunka.Execute.Control
import Biegunka.Verify (verify)


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links the whole ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt :: FilePath -> Script Files
registerAt dst = liftF $ EF (Link mempty dst) ()
{-# INLINE registerAt #-}


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link :: FilePath -> FilePath -> Script Files
link src dst = liftF $ EF (Link src dst) ()
{-# INLINE link #-}


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy :: FilePath -> FilePath -> Script Files
copy src dst = liftF $ EF (Copy src dst) ()
{-# INLINE copy #-}


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute :: FilePath -> FilePath -> Script Files
substitute src dst = liftF $
  EF (Template src dst (\b -> render . setAttribute "template" b . newSTMP)) ()
{-# INLINE substitute #-}


-- | Executes shell command with default shell
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   shell "echo -n hello"
--
-- Prints "hello" (without a newline)
shell :: String -> Script Files
shell c = liftF $ EF (Shell mempty c) ()
{-# INLINE shell #-}


-- | Change effective user id for wrapped commands
sudo :: String -> Free (EL l s) () -> Free (EL l s) ()
sudo n s = liftF (EW (User (Just n)) ()) >> s >> liftF (EW (User Nothing) ())
{-# INLINE sudo #-}


-- | Change reaction pattern for wrapped commands
reacting :: React -> Free (EL l s) () -> Free (EL l s) ()
reacting r s = liftF (EW (Reacting (Just r)) ()) >> s >> liftF (EW (Reacting Nothing) ())
{-# INLINE reacting #-}


-- | Configuration profile
--
-- Provides convenient sources grouping
--
-- > profile "mine" $ do
-- >   git ...
-- >   git ...
-- > profile "friend's" $ do
-- >   svn ...
profile :: String -> Script Sources -> Script Profiles
profile name repo = liftF $ EP name repo ()
{-# INLINE profile #-}


-- | Concurrent task
-- Runs in parallel with main thread if possible
task :: Free (EL l s) () -> Free (EL l s) ()
task s = liftF (EW (Task True) ()) >> s >> liftF (EW (Task False) ())
{-# INLINE task #-}
