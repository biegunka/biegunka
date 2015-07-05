{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Support for git repositories as 'Sources'
module Control.Biegunka.Source.Git.Internal
  ( git
  , Git
  , Repository
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
import           Control.Biegunka.Source (Repository, HasPath(..), HasUrl(..))



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
git :: Git Repository FilePath -> Script 'Actions () -> Script 'Sources ()
git f = sourced Source
  { sourceType   = "git"
  , sourceFrom   = configUrl
  , sourceTo     = configPath
  , sourceUpdate = update config
  }
 where
  config@Config { configUrl, configPath } =
    f defaultConfig

type Git a b = Config () () -> Config a b

data Config a b = Config
  { configUrl         :: a
  , configPath        :: b
  , configBranch      :: String
  , configFailIfAhead :: Bool
  }

defaultConfig :: Config () ()
defaultConfig = Config
  { configUrl         = ()
  , configPath        = ()
  , configBranch      = "master"
  , configFailIfAhead = False
  }

instance HasUrl (Config a b) (Config Repository b) Repository where
  url u config = config { configUrl = u }

instance HasPath (Config a b) (Config a FilePath) FilePath where
  path p config = config { configPath = p }

-- | Set git branch to track.
branch :: String -> Config a b -> Config a b
branch b config = config { configBranch = b }

-- | Fail when the are local commits ahead of the tracked remote branch.
failIfAhead :: Config a b -> Config a b
failIfAhead config = config { configFailIfAhead = True }

update :: Config Repository a -> FilePath -> IO (Maybe String, IO (Maybe String))
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
      return
        ( bool (Just (printf "‘%s’ → ‘%s’" before after)) Nothing (before == after)
        , do
          currentBranch <- fmap (listToMaybe . lines)
                                (runGit fp ["rev-parse", "--abbrev-ref", "HEAD"])
          assertBranch configBranch currentBranch
          ahead <- fmap (not . null . lines)
                        (runGit fp ["rev-list", rbr ++ ".." ++ configBranch])
          if ahead && configFailIfAhead
            then sourceFailure "local branch is ahead of remote"
            else Nothing <$ runGit fp ["rebase", rbr]
        )
    False ->
      return
        ( Just "first checkout"
        , do runGit "/" ["clone", configUrl, fp, "-b", configBranch]
             after <- gitHash fp "HEAD"
             return (Just (printf "‘none’ → ‘%s’" after))
        )

assertBranch :: String -> Maybe String -> IO ()
assertBranch remoteBranch = \case
    Just currentBranch
      | currentBranch == remoteBranch -> return ()
      | otherwise ->
        sourceFailure $ "The wrong branch is checked out.\nExpected: ‘" ++ remoteBranch ++ "’\n But got: ‘" ++ currentBranch ++ "’"
    Nothing ->
      sourceFailure "Unable to determine what branch is checked out."

assertUrl :: URI -> FilePath -> IO ()
assertUrl remoteURI p =
  listToMaybe . lines <$> runGit p ["config", "--get", "remote.origin.url"] >>= \case
    Just currentURI
      | currentURI == remoteURI -> return ()
      | otherwise ->
        sourceFailure $ "The ‘origin’ remote points to the wrong repository.\nExpected: ‘" ++ remoteURI ++ "’\n But got: ‘" ++ currentURI ++ "’"
    Nothing ->
      sourceFailure "Unable to determine what repository the ‘origin’ remote points to."


gitHash :: FilePath -> String -> IO String
gitHash fp ref = runGit fp ["rev-parse", "--short", ref]

runGit :: FilePath -> [String] -> IO String
runGit cwd args = Text.unpack . Text.stripEnd <$> do
  (exitcode, out, err) <- P.readCreateProcessWithExitCode proc ""
  exitcode `onFailure` \_ -> sourceFailure err
  return (Text.pack out)
 where
  proc = (P.proc "git" args) { P.cwd = Just cwd }
