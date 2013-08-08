{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git
  ( -- * Source layer
    git', git, git_
    -- * Types
  , Git(..), defaultGit
    -- ** Lenses
  , actions, remotes, branch
    -- ** Type synonyms
    -- $synonyms
  , Branch, Remote, URI
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.Foldable (for_)
import Data.Monoid (mempty)

import           Control.Lens
import           Data.Default (Default(..))
import qualified Data.Text as T
import           System.Directory (getCurrentDirectory, setCurrentDirectory, doesDirectoryExist)
import           System.FilePath ((</>))
import           System.Process (readProcessWithExitCode)

import Control.Biegunka.Execute.Exception (onFailure, sourceFailure)
import Control.Biegunka.Language (Scope(..))
import Control.Biegunka.Script
import Control.Biegunka.Source (Sourceable(..))


-- | Git repository's settings
data Git = Git
  { gitactions :: Script Actions () -- ^ Actions to run after repository update
  , _remotes   :: [Remote]          -- ^ Remotes to merge on update
  , _branch    :: Branch            -- ^ Branch to track
  }

instance Default Git where
  def = defaultGit

instance Sourceable Git where
  actions f x = f (gitactions x) <&> \as -> x { gitactions = as }

  (==>) = git'

-- | Do nothing except pulling @origin/master@ into @master@
defaultGit :: Git
defaultGit = Git
  { gitactions = def
  , _remotes   = ["origin"]
  , _branch    = "master"
  }

-- | Remotes to merge on update
remotes :: Lens' Git [Remote]
remotes f x = f (_remotes x) <&> \rs -> x { _remotes = rs }

-- | Branch to track
branch :: Lens' Git Branch
branch f x = f (_branch x) <&> \b -> x { _branch = b }


-- $synonyms
-- Convenient self-described types to remind yourself where is which argument

-- | Branch name (like @master@ or @develop@)
type Branch = String

-- | Remote name (like @origin@ or @upstream@)
type Remote = String


-- | Clone repository from the url to the specified path using provided 'Git' settings. Sample:
--
-- @
-- git' \"git\@github.com:edwinb\/Idris-dev\" \"git\/Idris-dev\" $ def
--   & remotes .~ [\"origin\", \"stream\"]
--   & branch .~ \"develop\"
--   & actions .~ do
--       link \"contribs\/tool-support\/vim\" \".vim\/bundle\/idris-vim\"
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
git' :: Target p => URI -> p -> Git -> Script Sources ()
git' url path (Git { gitactions, _remotes, _branch }) =
  sourced "git" url path gitactions (updateGit url _remotes _branch)
{-# INLINE git' #-}

-- | Wrapper over 'git'' that provides easy specification of 'actions' field
git :: Target p => URI -> p -> Script Actions () -> Script Sources ()
git u p s = git' u p def { gitactions = s }
{-# INLINE git #-}

-- | Wrapper over 'git' that does not provide anything
git_ :: Target p => URI -> p -> Script Sources ()
git_ u p = git u p def
{-# INLINE git_ #-}


updateGit :: URI -> [Remote] -> Branch -> FilePath -> IO ()
updateGit u rs br p = do
  exists <- doesDirectoryExist p
  if exists
    then do
      readGitProcess ["remote", "update"] (Just p)
      forM_ rs $ \r ->
        readGitProcess ["merge", r </> br, br] (Just p)
    else
      readGitProcess ["clone", u, p] Nothing
  readGitProcess ["checkout", br] (Just p)
 where
  readGitProcess args workingDirectory = bracket
    getCurrentDirectory
    setCurrentDirectory $ \_ -> do
      for_ workingDirectory setCurrentDirectory
      (exitcode, _, errors) <- readProcessWithExitCode "git" args mempty
      exitcode `onFailure`
        \_ -> sourceFailure u p (T.pack errors)
