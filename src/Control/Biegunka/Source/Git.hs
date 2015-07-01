{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git
  ( -- * Source layer
    git', git, git_
    -- * Types
  , Git
    -- * Modifiers
  , actions, remote, branch
    -- * Type synonyms
  , URI
  ) where

import           Data.Bool (bool)
import qualified Data.Text as Text
import           System.Directory (doesDirectoryExist)
import           System.FilePath ((</>))
import qualified System.Process as P
import           Text.Printf (printf)

import Control.Biegunka.Execute.Exception (onFailure, sourceFailure)
import Control.Biegunka.Language (Scope(..))
import Control.Biegunka.Script
import Control.Biegunka.Source (Sourceable(..))


-- | Git repository's settings
data Git = Git
  { _actions :: Script 'Actions () -- ^ Actions to run after repository update
  , _remote  :: String             -- ^ Remote to track.
  , _branch  :: String             -- ^ Branch to track.
  }

instance Sourceable Git where
  newtype Mod Git = Mod (Git -> Git)
  actions y = Mod (\x -> x { _actions = y })

  (==>) = git'

instance Monoid (Mod Git) where
  mempty = Mod id
  Mod f `mappend` Mod g = Mod (g . f)

-- | Do nothing except pulling @origin/master@ into @master@
defaultGit :: Git
defaultGit = Git
  { _actions = return ()
  , _remote  = "origin"
  , _branch  = "master"
  }

-- | Set git remote to track.
remote :: String -> Mod Git
remote y = Mod (\x -> x { _remote = y })

-- | Set git branch to track.
branch :: String -> Mod Git
branch y = Mod (\x -> x { _branch = y })


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
git' :: URI -> FilePath -> Mod Git -> Script 'Sources ()
git' url path (Mod f) =
  sourced "git" url path _actions (\p -> updateGit url p g)
 where
  g@Git { _actions } = f defaultGit

-- | Wrapper over 'git'' that provides easy specification of 'actions' field
git :: URI -> FilePath -> Script 'Actions () -> Script 'Sources ()
git u p s = git' u p (actions s)

-- | Wrapper over 'git' that does not provide anything
git_ :: URI -> FilePath -> Script 'Sources ()
git_ u p = git' u p mempty

updateGit :: URI -> FilePath -> Git -> IO (Maybe String)
updateGit u p Git { _remote, _branch } =
  doesDirectoryExist p >>= \case
    True -> do
      let rbr = _remote </> _branch
      before <- gitHash
      -- Typical git: <https://stackoverflow.com/questions/11347712/git-fetch-only-for-current-branch>
      _ <- askGit p ["fetch", _remote, _branch] -- TODO: track upstream
      _ <- askGit p ["checkout", "-B", _branch, "--track", rbr] -- TODO: track upstream
      _ <- askGit p ["merge", rbr, _branch] -- TODO: rebase
      after <- gitHash
      return (bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after))
    False -> do
      _ <- askGit "/" ["clone", u, p, "-b", _branch]
      after <- gitHash
      return (Just (printf "checked ‘%s’ out" after))
 where
  gitHash = fmap (Text.unpack . Text.stripEnd) (askGit p ["rev-parse", "--short", "HEAD"])

  askGit cwd args = do
    let proc = P.proc "git" args
    (exitcode, out, err) <-
      P.readCreateProcessWithExitCode proc { P.cwd = Just cwd } ""
    exitcode `onFailure` \_ -> sourceFailure (Text.pack err)
    return (Text.pack out)
