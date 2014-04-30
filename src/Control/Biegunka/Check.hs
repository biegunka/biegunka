{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Check interpreter
module Control.Biegunka.Check (check) where

import           Control.Exception (bracket)
import           Control.Lens hiding (Action)
import           Control.Monad.Free (Free, iter)
import           System.FilePath (splitFileName, splitDirectories, makeRelative)
import           System.Directory.Layout (Layout)
import qualified System.Directory.Layout as Layout
import           System.IO (Handle, hGetContents, hClose)
import           System.Exit (ExitCode(..))
import qualified System.Posix as Posix
import           Test.Hspec.Formatters (progress)
import           Test.Hspec.Runner (hspecWith, defaultConfig, Config(..), ColorMode(..), summaryFailures)

import           Control.Biegunka.Biegunka (Interpreter, interpret)
import           Control.Biegunka.Language
import           Control.Biegunka.Script

check :: Interpreter
check = interpret $ \settings terms k -> do
  (infd, outfd) <- Posix.createPipe
  withFd infd $ \inh -> do
    s <- withFd outfd $ \outh ->
      hspecWith (defaultConfig
        { configFormatter = progress, configColorMode = ColorAlways, configHandle = Left outh }) $
        Layout.spec (view root settings) (termsLayout (view root settings) terms)
    hGetContents inh >>= putStrLn
    case summaryFailures s of
      0 -> k
      _ -> return (ExitFailure 1)

withFd :: Posix.Fd -> (Handle -> IO a) -> IO a
withFd fd = bracket (Posix.fdToHandle fd) hClose

termsLayout :: FilePath -> Free (Term Annotate s) () -> Layout ()
termsLayout p = iter go . fmap return where
  go (TS (AS { asUser }) (Source { spath }) innards spec) = do
    Layout.emptydir (rel spath)
      & Layout.user .~ asUser
    termsLayout p innards
    spec
  go (TA (AA { aaUser }) action spec) = do
    case action of
      Link file target ->
        case split (rel target) of
          ~(ds, f) ->
            Layout.dirs ds $
              Layout.symlink f file
                & Layout.user .~ aaUser
                & Layout.exists .~ True
      Copy file target _ ->
        case split (rel target) of
          ~(ds, f) ->
            Layout.dirs ds $
              Layout.file f
                & Layout.contents ?~ Layout.copyOf file
                & Layout.user .~ aaUser
      Template _ target _ ->
        case split (rel target) of
          ~(ds, f) ->
            Layout.dirs ds $
              Layout.file f
                & Layout.user .~ aaUser
      Patch {} ->
        return ()
      Command {} ->
        return ()
    spec
  go (TM _ spec) = spec

  rel = makeRelative p

-- | Split the filepath into the list of directories and the filename
split :: FilePath -> ([FilePath], FilePath)
split path = let ~(ds, f) = splitFileName path in (splitDirectories ds, f)
