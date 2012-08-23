{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Verify (verify) where

import Control.Applicative (liftA2)
import Control.Monad (unless)

import           Control.Monad.Free (Free(..))
import           Control.Monad.State (StateT, evalStateT)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink)

import Biegunka.State
import Biegunka.DSL.Profile (Profile(..))
import Biegunka.DSL.Repository (Repository(..))
import Biegunka.DSL.Files (Files(..))


verify ∷ StateT BiegunkaState (Free Profile) () → IO ()
verify script = do
  home ← getHomeDirectory
  let state = BiegunkaState { _root = home, _repositoryRoot = ""}
  verified ← profile state (evalStateT script state)
  putStr "Verified... "
  if verified
    then putStrLn "OK"
    else putStrLn "Fail!"


profile ∷ BiegunkaState → Free Profile () → IO Bool
profile state (Free (Profile _ script next)) =
  repo state (evalStateT script state) |&&| profile state next
profile _ (Pure _) = return True


repo ∷ BiegunkaState → Free Repository () → IO Bool
repo state (Free (Git _ path script next)) = do
  repoExists ← doesDirectoryExist path
  if repoExists
    then files (evalStateT script state { _repositoryRoot = path }) |&&| repo state next
    else do
      putStrLn $ "  Repository " ++ path ++ " does not exist"
      return False |&&| repo state next
repo _ (Pure _) = return True


files ∷ Free Files () → IO Bool
files (Free (Message _ next)) = files next
files (Free (RegisterAt _ dst next)) = do
  repoExists ← doesDirectoryExist dst
  unless repoExists (putStrLn $ "    Repository link at " ++ dst ++ " does not exist")
  return repoExists |&&| files next
files (Free (Link src dst next)) = do
  src' ← readSymbolicLink dst
  let correctLink = src == src'
  unless correctLink (putStrLn brokenLink)
  return correctLink |&&| files next
 where
  brokenLink = "    Link at " ++ dst ++ " is broken/does not exist"
files (Free (Copy src dst next)) = do
  src' ← B.readFile src
  dst' ← B.readFile dst
  let same = src' == dst'
  unless same (putStrLn $ "    Files at " ++ src ++ " and " ++ dst ++ " are not copies")
  return same |&&| files next
files (Free (Compile _ _ dst next)) = do
  binaryExists ← doesFileExist dst
  unless binaryExists (putStrLn $ "    Compiled binary file at " ++ dst ++ " does not exist")
  return binaryExists |&&| files next
files (Pure _) = return True


(|&&|) ∷ IO Bool → IO Bool → IO Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|
