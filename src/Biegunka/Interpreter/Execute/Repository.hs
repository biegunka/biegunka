{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Repository (execute) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Data.Monoid (Monoid(..))

import Control.Monad.Free (Free(..))
import Control.Monad.State (evalStateT)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), hFlush, stdout, withFile)
import System.Process (runProcess, waitForProcess)

import Biegunka.DSL.Repository (Repository(..))
import qualified Biegunka.Interpreter.Execute.Files as Files


execute ∷ Free (Repository a) b → IO ()
execute (Free (Git url path script next)) =
  do update url path
     Files.execute (evalStateT script ()) path
     execute next
execute (Pure _) = return mempty


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
