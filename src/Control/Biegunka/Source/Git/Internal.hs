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
  , runGit, updateGit
  , gitHash
  , defaultGit
  ) where

import           Control.Monad (void, when)
import           Data.Bool (bool)
import           Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import           System.Directory (doesDirectoryExist)
import           System.FilePath ((</>))
import qualified System.Process as P
import           Text.Printf (printf)

import           Control.Biegunka.Execute.Exception (onFailure, sourceFailure)
import           Control.Biegunka.Language (Scope(..), Source(..))
import           Control.Biegunka.Script
import           Control.Biegunka.Source (Sourceable(..))


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
failIfAhead :: Mod Git
failIfAhead = Mod (\x -> x { _failIfAhead = True })


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
git' url path (Mod f) = sourced Source
  { sourceType   = "git"
  , sourceFrom   = url
  , sourceTo     = path
  , sourceUpdate = \p -> updateGit url p g
  } _actions
 where
  g@Git { _actions } = f defaultGit

-- | Wrapper over 'git'' that provides easy specification of 'actions' field
git :: URI -> FilePath -> Script 'Actions () -> Script 'Sources ()
git u p s = git' u p (actions s)

-- | Wrapper over 'git' that does not provide anything
git_ :: URI -> FilePath -> Script 'Sources ()
git_ u p = git' u p mempty

updateGit :: URI -> FilePath -> Git -> IO (Maybe String, IO (Maybe String))
updateGit u p Git { _branch, _failIfAhead } =
  doesDirectoryExist p >>= \case
    True -> do
      let rbr = "origin" </> _branch
      before <- gitHash p "HEAD"
      remotes <- lines `fmap` runGit p ["remote"]
      when ("origin" `notElem` remotes)
           (void (runGit p ["remote", "add", "origin", u]))
      runGit p ["fetch", "origin", _branch]
      after <- gitHash p rbr
      return
        ( bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after)
        , do
          currentBranch <- fmap (listToMaybe . lines)
                                (runGit p ["rev-parse", "--abbrev-ref", "HEAD"])
          when (currentBranch /= Just _branch)
               (void (runGit p ["checkout", "-B", _branch, "--track", rbr]))
          ahead <- fmap (not . null . lines)
                        (runGit p ["rev-list", rbr ++ ".." ++ _branch])
          if ahead && _failIfAhead
            then sourceFailure "local branch is ahead of remote"
            else Nothing <$ runGit p ["rebase", rbr]
        )
    False ->
      return
        ( Just "first checkout"
        , do runGit "/" ["clone", u, p, "-b", _branch]
             after <- gitHash p "HEAD"
             return (Just (printf "‘none’ → ‘%s’" after))
        )

gitHash :: FilePath -> String -> IO String
gitHash path ref = runGit path ["rev-parse", "--short", ref]

runGit :: FilePath -> [String] -> IO String
runGit cwd args = Text.unpack . Text.stripEnd <$> do
  (exitcode, out, err) <- P.readCreateProcessWithExitCode proc ""
  exitcode `onFailure` \_ -> sourceFailure err
  return (Text.pack out)
 where
  proc = (P.proc "git" args) { P.cwd = Just cwd }
