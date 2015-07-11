{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | Check interpreter
module Control.Biegunka.Check (check) where

import           Control.Concurrent.Async (async, waitCatch)
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Free (iter)
import           System.FilePath (splitFileName, splitDirectories, makeRelative)
import           System.Directory.Layout (Layout)
import qualified System.Directory.Layout as Layout
import qualified System.IO as IO
import           System.Environment (withArgs, withProgName)
import           System.Exit (ExitCode(..))
import qualified System.Posix as Posix
import           Test.Hspec.Formatters (progress)
import           Test.Hspec.Runner (hspecWithResult, defaultConfig, Config(..), ColorMode(..), summaryFailures)

import           Control.Biegunka.Interpreter (Interpreter(I))
import           Control.Biegunka.Language
import qualified Control.Biegunka.Logger as Logger
import           Control.Biegunka.Script

check :: Interpreter
check = I $ \settings terms k -> do
  (infd, outfd) <- Posix.createPipe
  withFd infd $ \inh -> do
    a <- async . forever $
      IO.hGetLine inh >>=
        Logger.write IO.stdout settings . (++ "\n")
    s <- withFd outfd $ \outh -> do
      let rr = view runRoot settings
      IO.hSetBuffering outh IO.LineBuffering
      withProgName "biegunka" .  withArgs [] $
        hspecWithResult defaultConfig
          { configFormatter  = Just progress
          , configColorMode  = ColorAlways
          , configOutputFile = Left outh
          }
          (Layout.examples rr (term rr terms))
    waitCatch a
    case summaryFailures s of
      0 -> k
      n -> return (ExitFailure n)

withFd :: Posix.Fd -> (IO.Handle -> IO a) -> IO a
withFd fd = bracket (Posix.fdToHandle fd) IO.hClose

term :: FilePath -> Term Annotate s () -> Layout ()
term p = iter go . fmap return where
  go (TS AS { asUser } Source { sourceTo } innards spec) = do
    Layout.emptydir (rel sourceTo)
      & Layout.user .~ asUser
    term p innards
    spec
  go (TF AA { aaUser } tf spec) = do
      node tf
      spec
   where
    node :: File t FilePath FilePath -> Layout ()
    node file = let
        origin_ = view origin file
        path_ = view path file
      in case file of
          FC {} -> case split (rel path_) of
              ~(ds, f) ->
                Layout.dirs ds $
                  Layout.file f
                    & Layout.contents ?~ Layout.copyOf origin_
                    & Layout.user .~ aaUser
          FT {} -> case split (rel path_) of
              ~(ds, f) ->
                Layout.dirs ds $
                  Layout.file f
                    & Layout.user .~ aaUser
          FL {} -> case split (rel path_) of
              ~(ds, f) ->
                Layout.dirs ds $
                  Layout.symlink f origin_
                    & Layout.user .~ aaUser
                    & Layout.exists .~ True
  go (TC _ _ spec) = spec
  go (TW _ spec) = spec

  rel = makeRelative p

-- | Split the filepath into the list of directories and the filename
split :: FilePath -> ([FilePath], FilePath)
split fp = let ~(ds, f) = splitFileName fp in (splitDirectories ds, f)
