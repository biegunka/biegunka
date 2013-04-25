{-# LANGUAGE DataKinds #-}
-- | Biegunka - configuration management library
module Biegunka
  ( -- * Interpreters control
    biegunka, Controls, root, appData, pretty, Pretty(..)
    -- * Interpreters
  , pretend, pause, execute, verify
    -- * Execution environment hooks
  , EE
  , Priviledges(..), priviledges
  , React(..), react
  , Templates(..), templates
  , retries
    -- * All scopes
  , sudo, reacting, chain, (~>>)
    -- * Profile scope
  , profile
    -- * File scope
  , registerAt, copy, link, substitute, shell
    -- * For user to be able to write type signatures
  , Script, Scope(..)
  ) where

import Data.Monoid (mempty)

import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Control (biegunka, Controls, root, appData, pretty, Pretty(..), pause)
import Biegunka.Language (Scope(..), Script(..), lift, EL(..), A(..), W(..), React(..))
import Biegunka.Pretend (pretend)
import Biegunka.Execute (execute)
import Biegunka.Execute.Control
import Biegunka.Verify (verify)

infixr 7 `chain`, ~>>


-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links the whole ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt :: FilePath -> Script Actions ()
registerAt dst = lift $ EA (Link mempty dst) ()
{-# INLINE registerAt #-}


-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link :: FilePath -> FilePath -> Script Actions ()
link src dst = lift $ EA (Link src dst) ()
{-# INLINE link #-}


-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy :: FilePath -> FilePath -> Script Actions ()
copy src dst = lift $ EA (Copy src dst) ()
{-# INLINE copy #-}


-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute :: FilePath -> FilePath -> Script Actions ()
substitute src dst = lift $
  EA (Template src dst (\b -> render . setAttribute "template" b . newSTMP)) ()
{-# INLINE substitute #-}


-- | Executes shell command with default shell
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   shell "echo -n hello"
--
-- Prints "hello" (without a newline)
shell :: String -> Script Actions ()
shell c = lift $ EA (Shell mempty c) ()
{-# INLINE shell #-}


-- | Change effective user id for wrapped commands
sudo :: String -> Script sc () -> Script sc ()
sudo n s = lift (EW (User (Just n)) ()) >> s >> lift (EW (User Nothing) ())
{-# INLINE sudo #-}


-- | Change reaction pattern for wrapped commands
reacting :: React -> Script sc () -> Script sc ()
reacting r s = lift (EW (Reacting (Just r)) ()) >> s >> lift (EW (Reacting Nothing) ())
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
profile :: String -> Script Sources () -> Script Profiles ()
profile name repo = lift $ EP name repo ()
{-# INLINE profile #-}


-- | Chain tasks sequentially
-- Connects two tasks which forces them to run sequentially one after another.
--
-- Note: redundant if 'Order' is 'Sequential'
chain :: Script sc () -> Script sc () -> Script sc ()
chain a b = a >> lift (EW Chain ()) >> b
{-# INLINE chain #-}


-- | Alias for 'chain'
(~>>) :: Script sc () -> Script sc () -> Script sc ()
(~>>) = chain
{-# INLINE (~>>) #-}
