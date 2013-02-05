{-# LANGUAGE DataKinds #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root
    -- * Interpreters
  , pretend, pause, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Volubility(..), volubility
  , Templates(..), templates
    -- * All layers
  , sudo, reacting
    -- * Profile layer
  , profile
    -- * Source layer
  , task
    -- * File layer
  , registerAt, copy, link, substitute, shell
    -- * Convenient type aliases
  , Script, Layer(..)
  ) where

import Data.Monoid (mempty)

import Control.Monad.Free (Free(..), liftF)
import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Control (biegunka, Controls, root)
import Biegunka.Language (Script, Layer(..), Command(..), Action(..), Wrapper(..), React(..))
import Biegunka.Pretend (pause, pretend)
import Biegunka.Execute (execute)
import Biegunka.Execute.Narrator (Volubility(..))
import Biegunka.Execute.State
import Biegunka.Verify (verify)


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt :: FilePath -> Script Files
registerAt dst = liftF $ F (RegisterAt mempty dst) ()


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link :: FilePath -> FilePath -> Script Files
link src dst = liftF $ F (Link src dst) ()


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy :: FilePath -> FilePath -> Script Files
copy src dst = liftF $ F (Copy src dst) ()


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute :: FilePath -> FilePath -> Script Files
substitute src dst = liftF $
  F (Template src dst (\b -> render . setAttribute "template" b . newSTMP)) ()


-- | Executes shell command with default shell
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   shell "echo -n hello"
--
-- Prints "hello" (without a newline)
shell :: String -> Script Files
shell c = liftF $ F (Shell mempty c) ()


-- | Change effective user id for wrapped commands
sudo :: String -> Free (Command l s) () -> Free (Command l s) ()
sudo n s = liftF (W (User (Just n)) ()) >> s >> liftF (W (User Nothing) ())


-- | Change reaction pattern for wrapped commands
reacting :: React -> Free (Command l s) () -> Free (Command l s) ()
reacting r s = liftF (W (Reacting (Just r)) ()) >> s >> liftF (W (Reacting Nothing) ())


-- | Configuration profile
--
-- Provides convenient sources grouping
--
-- > profile "mine" $ do
-- >   git ...
-- >   git ...
-- > profile "friend's" $ do
-- >   svn ...
profile :: String -> Script Source -> Script Profile
profile name repo = liftF $ P name repo ()


-- | Concurrent task
-- Runs in parallel with main thread if possible. Currently defunct
task :: Script Source -> Script Source
task = id
{-# INLINE task #-}
