{-# LANGUAGE DataKinds #-}
-- | Language primitives
module Biegunka.Primitive
  ( -- * Profile layer primitives
    profile
    -- * Actions layer primitives
  , registerAt, link, copy, substitute, shell
    -- * Wrappers
  , sudo, reacting, chain, (<~>)
  ) where

import Data.Monoid (mempty)

import Control.Lens
import Control.Monad.State
import Control.Monad.Free (liftF)
import System.FilePath ((</>))
import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Language
import Biegunka.Script

infixr 7 `chain`, <~>


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
profile n i = Script $ do
  tok <- use token
  ast <- annotate i
  lift . liftF $ EP (SAP { sapToken = tok }) (P n) ast ()
  token += 1
{-# INLINE profile #-}

-- | Links source to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   registerAt "we/need/you/here"
--
-- Links the whole ${HOME}\/git\/repo to ${HOME}\/we\/need\/you\/here
registerAt :: FilePath -> Script Actions ()
registerAt dst = actioned (\rfp _ -> Link mempty (rfp </> dst))
{-# INLINE registerAt #-}

-- | Links given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   link "you" "we/need/you/here"
--
-- Links ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
link :: FilePath -> FilePath -> Script Actions ()
link src dst = actioned (\rfp sfp -> Link (sfp </> src) (constructDestinationFilepath rfp src dst))
{-# INLINE link #-}

-- | Copies given file to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   copy "you" "we/need/you/here"
--
-- Copies ${HOME}\/git\/repo\/you to ${HOME}\/we\/need\/you\/here
copy :: FilePath -> FilePath -> Script Actions ()
copy src dst = actioned (\rfp sfp -> Copy (sfp </> src) (constructDestinationFilepath rfp src dst))
{-# INLINE copy #-}

-- | Substitutes $template.X$ templates in given file and writes result to specified filepath
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   substitute "you.hs" "we/need/you/here"
--
-- Substitutes templates in ${HOME}\/git\/repo\/you.hs with values from
-- Settings.template and writes result to ${HOME}\/we\/need\/you\/here
substitute :: FilePath -> FilePath -> Script Actions ()
substitute src dst = actioned (\rfp sfp ->
  Template (sfp </> src) (constructDestinationFilepath rfp src dst)
    (\b -> render . setAttribute "template" b . newSTMP))
{-# INLINE substitute #-}


-- | Executes shell command with default shell
--
-- > git "https://example.com/repo.git" "git/repo" $
-- >   shell "echo -n hello"
--
-- Prints "hello" (without a newline)
shell :: String -> Script Actions ()
shell c = actioned (\_ sfp -> Shell sfp c)
{-# INLINE shell #-}

-- | Change effective user id for wrapped commands
sudo :: String -> Script sc () -> Script sc ()
sudo n s = liftS (EM (User (Just n)) ()) >> s >> liftS (EM (User Nothing) ())
{-# INLINE sudo #-}

-- | Change reaction pattern for wrapped commands
reacting :: React -> Script sc () -> Script sc ()
reacting r s = liftS (EM (Reacting (Just r)) ()) >> s >> liftS (EM (Reacting Nothing) ())
{-# INLINE reacting #-}

-- | Chain tasks sequentially
-- Connects two tasks which forces them to run sequentially one after another.
chain :: Script sc () -> Script sc () -> Script sc ()
chain a b = Script $ do
  s <- rewind token (annotate a >>= lift)
  t <- rewind token (annotate b >>= lift)
  token .= max s t

-- | Alias for 'chain'
(<~>) :: Script sc () -> Script sc () -> Script sc ()
(<~>) = chain
{-# INLINE (<~>) #-}
