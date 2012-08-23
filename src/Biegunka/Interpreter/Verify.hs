{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Verify (verify) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (unless)

import           Control.Monad.Free (Free(..))
import           Control.Monad.State (StateT, evalStateT)
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink)

import Biegunka.State
import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Source (Source(..))
import Biegunka.DSL.Files (Files(..))


verify ∷ StateT BiegunkaState (Free Profile) () → IO ()
verify script = do
  home ← getHomeDirectory
  let state = BiegunkaState { _root = home, _repositoryRoot = ""}
  (verified, failures) ← runWriterT (profile state (evalStateT script state))
  putStr "Verified... "
  if verified
    then do
      putStrLn "OK"
      putStrLn failures
    else do
      putStrLn "Fail!\n"
      putStrLn failures


profile ∷ BiegunkaState → Free Profile () → WriterT String IO Bool
profile state (Free (Profile _ script next)) =
  repo state (evalStateT script state) |&&| profile state next
profile _ (Pure _) = return True


repo ∷ BiegunkaState → Free Source () → WriterT String IO Bool
repo state (Free (Git _ path script next)) = do
  repoExists ← io $ doesDirectoryExist path
  if repoExists
    then files (evalStateT script state { _repositoryRoot = path }) |&&| repo state next
    else do
      tellLn $ "  Repository " ++ path ++ " does not exist"
      return False |&&| repo state next
repo _ (Pure _) = return True


files ∷ Free Files () → WriterT String IO Bool
files (Free (Message _ next)) = files next
files (Free (RegisterAt _ dst next)) = do
  repoExists ← io $ doesDirectoryExist dst
  unless repoExists (tellLn $ "    Repository link at " ++ dst ++ " does not exist")
  return repoExists |&&| files next
files (Free (Link src dst next)) = do
  src' ← io $ readSymbolicLink dst
  dstExists ← io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
  let correctLink = src == src' && dstExists
  unless correctLink (tellLn brokenLink)
  return correctLink |&&| files next
 where
  brokenLink = "    Link at " ++ dst ++ " is broken/destination does not exist"
files (Free (Copy src dst next)) = do
  src' ← io $ B.readFile src
  dst' ← io $ B.readFile dst
  let same = src' == dst'
  unless same (tellLn $ "    Files at " ++ src ++ " and " ++ dst ++ " are not copies")
  return same |&&| files next
files (Free (Compile _ _ dst next)) = do
  binaryExists ← io $ doesFileExist dst
  unless binaryExists (tellLn $ "    Compiled binary file at " ++ dst ++ " does not exist")
  return binaryExists |&&| files next
files (Pure _) = return True


(|&&|) ∷ Applicative m ⇒ m Bool → m Bool → m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io ∷ IO a → WriterT String IO a
io = liftIO


tellLn ∷ String → WriterT String IO ()
tellLn failure = tell $ failure ++ "\n"
