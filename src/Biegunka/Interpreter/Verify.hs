{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Verify (verify) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (unless)
import Data.Monoid (mconcat)

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import           Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink)

import Biegunka.DSL (ProfileScript, Profile(..), Source(..), Files(..))
import Biegunka.Interpreter.Common.State


verify ∷ Default s ⇒ ProfileScript s () → IO ()
verify script = do
  home ← getHomeDirectory
  let script' = infect home script
  (verified, failures) ← runWriterT (profile script')
  putStr "Verify… "
  if verified
    then putStrLn "OK"
    else putStrLn $ "Fail!\n" ++ failures


profile ∷ Free (Profile (Free (Source (Free Files ())) ())) ()
        → WriterT String IO Bool
profile (Free (Profile _ script next)) =
  repo script |&&| profile next
profile (Pure _) = return True


repo ∷ Free (Source (Free Files ())) ()
     → WriterT String IO Bool
repo (Free (Git _ path script next)) = do
  repoExists ← io $ doesDirectoryExist path
  if repoExists
    then files script |&&| repo next
    else do
      tellLn [indent 2, "Repository ", path, " does not exist"]
      return False |&&| repo next
repo (Pure _) = return True


files ∷ Free Files () → WriterT String IO Bool
files (Free (Message _ next)) = files next
files (Free (RegisterAt _ dst next)) = do
  repoExists ← io $ doesDirectoryExist dst
  unless repoExists $ tellLn [indent 4, "Repository link at ", dst, " does not exist"]
  return repoExists |&&| files next
files (Free (Link src dst next)) = do
  src' ← io $ readSymbolicLink dst
  dstExists ← io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
  let correctLink = src == src' && dstExists
  unless correctLink $ tellLn [indent 4, "Link at ", dst, " is broken"]
  return correctLink |&&| files next
files (Free (Copy src dst next)) = do
  src' ← io $ B.readFile src
  dst' ← io $ B.readFile dst
  let same = src' == dst'
  unless same $ tellLn [indent 4, "Files at ", src, " and ", dst, " are not copies"]
  return same |&&| files next
files (Free (Compile _ _ dst next)) = do
  binaryExists ← io $ doesFileExist dst
  unless binaryExists $ tellLn [indent 4, "Compiled binary file at ", dst, " does not exist"]
  return binaryExists |&&| files next
files (Pure _) = return True


(|&&|) ∷ Applicative m ⇒ m Bool → m Bool → m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io ∷ IO a → WriterT String IO a
io = liftIO


tellLn ∷ [String] → WriterT String IO ()
tellLn failure = tell . mconcat $ failure ++ ["\n"]


indent ∷ Int → String
indent n = replicate n ' '
