{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Source.Git
  ( git, git_
  ) where

import Control.Applicative ((<$>), (<*>))

import Control.Lens (uses)
import Control.Monad (unless)
import Control.Monad.Free (liftF)
import Control.Monad.Trans (lift)
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), hFlush, stdout, withFile)
import System.Process (runProcess, waitForProcess)

import Biegunka.Settings
import Biegunka.DSL (FileScript, Source(..), SourceScript)


git ∷ String → FilePath → FileScript s () → SourceScript s ()
git url path script = do
  sr ← uses root (</> path)
  lift . liftF $ Source url sr script (update url sr) ()


git_ ∷ String → FilePath → SourceScript s ()
git_ url path = git url path (return ())


update ∷ String → FilePath → IO ()
update u p =
  do exists ← (||) <$> doesDirectoryExist p <*> doesFileExist p
     unless exists $
       withProgressString ("Clone git repository from " ++ u ++ " to " ++ p ++ "… ") $
         withTempFile $ \h →
           waitForProcess =<< runProcess "git" ["clone", u, p] Nothing Nothing Nothing (Just h) (Just h)
     withProgressString ("Pulling in " ++ p ++ " from origin master… ") $
       withTempFile $ \h →
         waitForProcess =<< runProcess "git" ["pull", "origin", "master"] (Just p) Nothing Nothing (Just h) (Just h)
 where
  withTempFile = withFile "/tmp/biegunka.errors" WriteMode


withProgressString ∷ String → IO ExitCode → IO ()
withProgressString prompt action = do
  putStr prompt >> hFlush stdout
  result ← action
  case result of
    ExitSuccess → putStrLn "OK!"
    ExitFailure _ → putStrLn "Fail!" >> readFile "/tmp/biegunka.errors" >>= error


