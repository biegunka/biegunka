{-# LANGUAGE DataKinds #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root
    -- * Interpreters
  , pretend, pause, execute, executeWith, verify
    -- * Interpreters related
  , defaultExecution, templates, dropPriviledges
  , React(..), react, Volubility(..), volubility
    -- * Profile layer
  , profile
    -- * File layer
  , message, registerAt, copy, link, substitute
  , shell
  , sudo, reacting, ignorant, asking, abortive
    -- * Convenient type aliases
  , Script, Layer(..)
  ) where

import Data.Monoid (mempty)

import Control.Monad.Free (Free(..), liftF)
import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Control (biegunka, Controls, root)
import Biegunka.Language (Script, Layer(..), Command(..), Action(..), Wrapper(..), React(..))
import Biegunka.Pretend (pause, pretend)
import Biegunka.Execute
  ( execute, executeWith
  , defaultExecution, templates, dropPriviledges
  , react, Volubility(..), volubility
  )
import Biegunka.Verify (verify)


-- | Prints specified message to stdout
--
-- > message "hello!"
--
-- prints \"hello!\"
message :: String -> Script Files
message m = liftF $ F (Message m) ()


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


sudo :: String -> Free (Command l s) () -> Free (Command l s) ()
sudo name cs = liftF (W (User (Just name)) ()) >> cs >> liftF (W (User Nothing) ())


reacting :: React -> Free (Command l s) () -> Free (Command l s) ()
reacting r cs = liftF (W (Reacting (Just r)) ()) >> cs >> liftF (W (Reacting Nothing) ())

ignorant, asking, abortive :: Free (Command l s) () -> Free (Command l s) ()
ignorant = reacting Ignorant
asking   = reacting Asking
abortive = reacting Abortive


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
