{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute.Source (execute) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)

import Control.Monad.Free (Free(..))
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), hFlush, stdout, withFile)
import System.Process (runProcess, waitForProcess)

import Biegunka.DSL (Source(..), Files(..))
import qualified Biegunka.Interpreter.Execute.Files as Files


execute ∷ Free (Source (Free Files ())) ()
        → IO ()
execute (Free (Git url path script next)) =
  do update url path
     Files.execute script
     execute next
execute (Pure _) = return ()


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
