{-# LANGUAGE DataKinds #-}
-- | Language primitives
module Biegunka.Primitive
  ( -- * Profile layer primitives
    profile
    -- * Actions layer primitives
  , registerAt, link, copy, substitute, shell
    -- * Wrappers
  , sudo, reacting, chain, (~>>)
  ) where

import Data.Monoid (mempty)

import Text.StringTemplate (newSTMP, render, setAttribute)

import Biegunka.Language (Scope(..), Script(..), lift, EL(..), P(..), A(..), W(..), React(..))

infixr 7 `chain`, ~>>


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
profile n s = lift $ EP (Profile n) s ()
{-# INLINE profile #-}

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
