{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git.Internal
  ( Git
  , git
  , Url
  , Config(..)
  , url
  , path
  , branch
  , failIfAhead
  , update
  , defaultConfig
  , runGit
  , gitHash
  ) where

import           Control.Applicative (empty)
import           Control.Lens
import           Data.Bifunctor (Bifunctor(..))
import           Data.Bool (bool)
import qualified Data.List as List
import           Data.Maybe (listToMaybe)
import qualified Data.Text as Text
import           System.Directory (doesDirectoryExist)
import           System.Exit.Lens (_ExitFailure)
import           System.FilePath ((</>))
import qualified System.Process as P
import           Text.Printf (printf)

import           Control.Biegunka.Execute.Exception (sourceFailure)
import           Control.Biegunka.Language (Scope(..), Source(..), DiffItem(..), diffItemHeaderOnly)
import           Control.Biegunka.Script (Script, sourced)
import           Control.Biegunka.Source (Url, HasPath(..), HasUrl(..))



-- | Clone or update a clone of the git repository, then run a script on its contents.
--
-- An example:
--
-- @
-- git (url \"git\@github.com:edwinb\/Idris-dev\" . path \"git\/Idris-dev\" . branch \"develop\") $ do
--   link \"contribs\/tool-support\/vim\" \".vim\/bundle\/idris-vim\")
-- @
--
--  1. Clone the git repository at @https:\/\/github.com\/edwinb\/Idris-dev.git@ to @~\/git\/Idris-dev@.
--
--  2. Check out the @develop@ branch.
--
--  3. Link the @~\/git\/Idris-dev\/contribs\/tool-support\/vim@ directory to @~\/.vim\/bundle\/Idris-vim@
git :: Git Url FilePath -> Script 'Actions () -> Script 'Sources ()
git f = sourced Source
  { sourceType   = "git"
  , sourceFrom   = configUrl
  , sourceTo     = configPath
  , sourceUpdate = update config
  }
 where
  config@Config { configUrl, configPath } =
    f defaultConfig

type Git a b = Config NoUrl NoPath -> Config a b

data Config a b = Config
  { configUrl         :: a
  , configPath        :: b
  , configBranch      :: String
  , configFailIfAhead :: Bool
  } deriving (Functor)

instance Bifunctor Config where
  first f config = config { configUrl = f (configUrl config) }
  second = fmap

defaultConfig :: Config NoUrl NoPath
defaultConfig = Config
  { configUrl         = NoUrl
  , configPath        = NoPath
  , configBranch      = "master"
  , configFailIfAhead = False
  }

data NoUrl = NoUrl

data NoPath = NoPath

instance HasUrl (Config a b) (Config Url b) Url where
  url = first . const

instance HasPath (Config a b) (Config a FilePath) FilePath where
  path = second . const

-- | Set git branch to track.
branch :: String -> Config a b -> Config a b
branch b config = config { configBranch = b }

-- | Fail when the are local commits ahead of the tracked remote branch.
failIfAhead :: Config a b -> Config a b
failIfAhead config = config { configFailIfAhead = True }

update :: Config Url a -> FilePath -> IO ([DiffItem], IO [DiffItem])
update Config { configUrl, configBranch, configFailIfAhead } fp =
  doesDirectoryExist fp >>= \case
    True -> do
      let rbr = "origin" </> configBranch
      before <- gitHash fp "HEAD"
      remotes <- lines <$> runGit fp ["remote"]
      if "origin" `notElem` remotes
        then () <$ runGit fp ["remote", "add", "origin", configUrl]
        else assertUrl configUrl fp
      runGit fp ["fetch", "origin", configBranch]
      after <- gitHash fp rbr
      oneliners <- fmap lines (runGit fp ["log", "--pretty=‘%h’ %s", before ++ ".." ++ after])
      let gitDiff = DiffItem
            { diffItemHeader = printf "‘%s’ → ‘%s’" before after
            , diffItemBody   = List.intercalate "\n" oneliners
            }
      return
        ( bool (pure gitDiff) empty (before == after || null oneliners)
        , do
          currentBranch <- fmap (listToMaybe . lines)
                                (runGit fp ["rev-parse", "--abbrev-ref", "HEAD"])
          assertBranch configBranch currentBranch
          ahead <- fmap (not . null . lines)
                        (runGit fp ["rev-list", rbr ++ ".." ++ configBranch])
          if ahead && configFailIfAhead
            then sourceFailure "Failed because of the ‘failIfAhead’ flag being set.\nThere are commits ahead of the remote branch."
            else empty <$ runGit fp ["rebase", rbr]
        )
    False ->
      return
        ( pure (diffItemHeaderOnly "first checkout")
        , do runGit "/" ["clone", configUrl, fp, "-b", configBranch]
             after <- gitHash fp "HEAD"
             return (pure (diffItemHeaderOnly (printf "‘none’ → ‘%s’" after)))
        )

assertBranch :: String -> Maybe String -> IO ()
assertBranch remoteBranch = \case
    Just currentBranch
      | currentBranch == remoteBranch -> return ()
      | otherwise ->
        sourceFailure (printf "The wrong branch is checked out.\nExpected: ‘%s’\n But got: ‘%s’" remoteBranch currentBranch)
    Nothing ->
      sourceFailure "Unable to determine what branch is checked out."

assertUrl :: Url -> FilePath -> IO ()
assertUrl remoteUrl p =
  listToMaybe . lines <$> runGit p ["config", "--get", "remote.origin.url"] >>= \case
    Just currentUrl
      | currentUrl == remoteUrl -> return ()
      | otherwise ->
        sourceFailure (printf "The ‘origin’ remote points to the wrong repository.\nExpected: ‘%s’\n But got: ‘%s’" remoteUrl currentUrl)
    Nothing ->
      sourceFailure "Unable to determine what repository the ‘origin’ remote points to."


gitHash :: FilePath -> String -> IO String
gitHash fp ref = runGit fp ["rev-parse", "--short", ref]

runGit :: FilePath -> [String] -> IO String
runGit cwd args = Text.unpack . Text.stripEnd <$> do
  (exitcode, out, err) <- P.readCreateProcessWithExitCode proc ""
  forOf_ _ExitFailure exitcode (\_ -> sourceFailure err)
  return (Text.pack out)
 where
  proc = (P.proc "git" args) { P.cwd = Just cwd }
