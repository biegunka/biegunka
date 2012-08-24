{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Execute (execute) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.Free (Free(..))
import Data.Function (on)
import Data.Monoid (mempty)
import System.Exit (ExitCode(..))
import System.IO (IOMode(WriteMode), hFlush, stdout, withFile)

import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory
  ( copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getHomeDirectory, removeDirectoryRecursive, removeFile
  )
import           System.FilePath (dropFileName, splitFileName)
import           System.Posix.Files (createSymbolicLink)
import           System.Process (runProcess, waitForProcess)

import           Biegunka.DB (Biegunka(..), load, save)
import           Biegunka.DSL (ProfileScript, Profile(..), Source(..), Files(..), Compiler(..))
import qualified Biegunka.Interpreter.ConstructMap as Map
import           Biegunka.Interpreter.State


execute ∷ ProfileScript () → IO ()
execute script = do
  home ← getHomeDirectory
  let script' = infect home script
  Biegunka α ← load
  when (α == mempty) $
    putStrLn "Warning: Biegunka is empty"
  profile script'
  let β = Map.construct script'
  removeOrphanFiles α β
  removeOrphanRepos α β
  save $ Biegunka β
 where
  removeOrphanFiles = removeOrphan removeFile files'
  removeOrphanRepos = removeOrphan removeDirectoryRecursive repos

  removeOrphan f g = removeIfNotElem f `on` g
  removeIfNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) $ f x

  files' α = M.elems α >>= M.elems >>= S.toList
  repos α = M.elems α >>= M.keys


profile ∷ Free (Profile (Free (Source (Free Files ())) ())) () → IO ()
profile (Free (Profile _ repo next)) = source repo >> profile next
profile (Pure _) = return ()


source ∷ Free (Source (Free Files ())) () → IO ()
source (Free (Git url path script next)) =
  do update url path
     files script
     source next
source (Pure _) = return ()


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


files ∷ Free Files a → IO ()
files (Free (Message m x)) = putStrLn m >> files x
files (Free (RegisterAt src dst x)) = overWriteWith createSymbolicLink src dst >> files x
files (Free (Link src dst x)) = overWriteWith createSymbolicLink src dst >> files x
files (Free (Copy src dst x)) = overWriteWith copyFile src dst >> files x
files (Free (Compile cmp src dst x)) = compileWith cmp src dst >> files x
files (Pure _) = return ()


overWriteWith ∷ (FilePath → FilePath → IO ()) → FilePath → FilePath → IO ()
overWriteWith f src dst =
  do createDirectoryIfMissing True $ dropFileName dst
     try $ removeFile dst ∷ IO (Either SomeException ())
     f src dst


compileWith ∷ Compiler → FilePath → FilePath → IO ()
compileWith GHC src dst =
  do waitForProcess =<< runProcess "ghc" ["-O2", "--make", file, "-fforce-recomp", "-v0", "-o", dst] (Just dir) Nothing Nothing Nothing Nothing
     return ()
 where
  (dir, file) = splitFileName src
