{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
-- | Biegunka.Source.Git - support for git repositories as sources
module Biegunka.Source.Git
  ( -- * Source layer
    git, git'
    -- * Types
  , Git(..)
    -- ** Type synonyms
    -- $synonyms
  , Branch, Remote, URI
  ) where

import           Control.Monad (forM_)
import           System.Exit (ExitCode(..))

import           Data.Default (Default(..))
import qualified Data.Text.IO as T
import           System.Directory (doesDirectoryExist)
import           System.FilePath ((</>))
import           System.Posix.IO (createPipe, fdToHandle)
import           System.Process (runProcess, waitForProcess)

import Biegunka.Language
import Biegunka.Execute.Exception



-- | Git repository's settings
data Git = Git
  { actions :: Script Actions () -- ^ Actions to run after repository update
  , remotes :: [Remote]          -- ^ Remotes to merge on update
  , branch  :: Branch            -- ^ Branch to track
  }

-- | Do nothing except pulling @origin/master@ into @master@
instance Default Git where
  def = Git
    { actions = return ()
    , remotes = ["origin"]
    , branch  = "master"
    }

-- $synonyms
-- Convenient self-described types to remind yourself where is which argument

-- | Branch name (like @master@ or @develop@)
type Branch = String

-- | Remote name (like @origin@ or @upstream@)
type Remote = String

-- | Repository URI (like @git\@github.com:whoever/whatever.git@)
type URI = String


-- | Clone repository from the url to the specified path using provided 'Git' settings. Sample:
--
-- @
-- git' \"git\@github.com:edwinb\/Idris-dev\" \"git\/Idris-dev\" def
--   { remotes = [\"origin\", \"stream\"]
--   , branch = \"develop\"
--   , actions = do
--       link \"contribs\/tool-support\/vim\" \".vim\/bundle\/idris-vim\"
--   }
-- @
--
--  1. Clone repository from @https:\/\/github.com\/edwinb\/Idris-dev.git@ to @~\/git\/Idris-dev@
--
--  2. Merge @origin/develop@ into @develop@
--
--  3. Merge @stream/develop@ into @develop@
--
--  4. Checkout to @develop@
--
--  5. Link @~\/git\/Idris-dev\/contribs\/tool-support\/vim@ to @~\/.vim\/bundle\/Idris-vim@
git' :: URI -> FilePath -> Git -> Script Sources ()
git' u p (Git { actions, remotes, branch }) =
  lift $ ES (Source "git" u p (updateGit u remotes branch)) actions ()
{-# INLINE git' #-}

-- | Wrapper over 'git'' that provides easy specification of 'actions' field.
git :: URI -> FilePath -> Script Actions () -> Script Sources ()
git u p s = git' u p def { actions = s }
{-# INLINE git #-}


updateGit :: URI -> [Remote] -> Branch -> FilePath -> IO ()
updateGit u rs br p = do
  exists <- doesDirectoryExist p
  if exists
    then do
      gitie ["remote", "update"] (Just p)
      forM_ rs $ \r ->
        gitie ["merge", r </> br, br] (Just p)
    else
      gitie ["clone", u, p] Nothing
  gitie ["checkout", br] (Just p)
 where
  check ih (ExitFailure _) = do
    l <- T.hGetContents ih
    sourceFailure u p l
  check _ _ = return ()

  gitie xs fp = do
    (ifd,ofd) <- createPipe
    ih <- fdToHandle ifd
    oh <- fdToHandle ofd
    check ih =<< waitForProcess =<< runProcess "git" xs fp Nothing Nothing (Just oh) (Just oh)
