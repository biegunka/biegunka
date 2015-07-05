{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git.Internal
  ( git', git, git_
  , Git (..)
  , actions, branch, failIfAhead
  , URI
  , askGit, askGit', updateGit
  , gitHash
  , defaultGit
  ) where

import           Control.Monad                      (void, when)
import           Data.Bool                          (bool)
import           Data.Maybe                         (listToMaybe)
import qualified Data.Text                          as Text
import           System.Directory                   (doesDirectoryExist)
import           System.FilePath                    ((</>))
import qualified System.Process                     as P
import           Text.Printf                        (printf)

import           Control.Biegunka.Execute.Exception (onFailure, sourceFailure)
import           Control.Biegunka.Language          (Scope (..))
import           Control.Biegunka.Script
import           Control.Biegunka.Source            (Sourceable (..))


-- | Git repository's settings
data Git = Git
  { _actions     :: Script 'Actions () -- ^ Actions to run after repository update
  , _branch      :: String             -- ^ Branch to track.
  , _failIfAhead :: Bool               -- ^ Fail if local branch is ahead of remote tracking branch
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
  , _branch  = "master"
  , _failIfAhead = False
  }

-- | Set git branch to track.
branch :: String -> Mod Git
branch y = Mod (\x -> x { _branch = y })

-- | Set silent rebasing behavior.
failIfAhead :: Bool -> Mod Git
failIfAhead y = Mod (\x -> x { _failIfAhead = y })


-- | Clone repository from the url to the specified path using provided 'Git' settings. Sample:
--
-- @
-- git' \"git\@github.com:edwinb\/Idris-dev\" \"git\/Idris-dev\
--   (branch \"develop\") <>
--   (actions .~ do
--       link \"contribs\/tool-support\/vim\" \".vim\/bundle\/idris-vim\")
-- @
--
--  1. Clone repository from @https:\/\/github.com\/edwinb\/Idris-dev.git@ to @~\/git\/Idris-dev@
--
--  2. Checkout to @develop@
--
--  3. Link @~\/git\/Idris-dev\/contribs\/tool-support\/vim@ to @~\/.vim\/bundle\/Idris-vim@
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
updateGit u p Git { _branch, _failIfAhead } =
  doesDirectoryExist p >>= \case
    True -> do
      let rbr = "origin" </> _branch
      before <- gitHash p
      remotes <- lines `fmap` askGit p ["remote"]
      when ("origin" `notElem` remotes) $
        askGit' p ["remote", "add", "origin", u]
      -- Typical git: <https://stackoverflow.com/questions/11347712/git-fetch-only-for-current-branch>
      askGit' p ["fetch", "origin", _branch]
      currentBranch <- (listToMaybe . lines) `fmap` askGit p ["rev-parse", "--abbrev-ref", "HEAD"]
      when (currentBranch /= Just _branch) $
        askGit' p ["checkout", "-B", _branch, "--track", rbr]
      commitsAhead <- (not . null . lines) `fmap`
        askGit p ["rev-list", "origin/" ++ _branch ++ ".." ++ _branch]
      if (commitsAhead && _failIfAhead)
        then sourceFailure (Text.pack "local branch is ahead of remote")
        else askGit' p ["rebase", rbr]
      after <- gitHash p
      return (bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after))
    False -> do
      askGit' "/" ["clone", u, p, "-b", _branch]
      after <- gitHash p
      return (Just (printf "checked ‘%s’ out" after))

gitHash :: FilePath -> IO String
gitHash path = askGit path ["rev-parse", "--short", "HEAD"]

askGit' :: FilePath -> [String] -> IO ()
askGit' f args = void $ askGit f args

askGit :: FilePath -> [String] -> IO String
askGit cwd args = Text.unpack . Text.stripEnd <$> go
 where
  go = do
    let proc = P.proc "git" args
    (exitcode, out, err) <-
      P.readCreateProcessWithExitCode proc { P.cwd = Just cwd } ""
    exitcode `onFailure` \_ -> sourceFailure (Text.pack err)
    return (Text.pack out)
