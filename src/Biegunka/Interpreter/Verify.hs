{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Interpreter.Verify (verify) where

import Control.Applicative (Applicative, liftA2)
import Control.Monad (unless)
import Data.Monoid (mconcat)

import           Control.Lens ((^.))
import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import           Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink)

import Biegunka.DSL
  ( ProfileScript
  , Profile(..)
  , Source, from, to, script
  , Files(..)
  , foldie)
import Biegunka.Interpreter.Common.State


-- | Verify interpreter
--
-- Compares current filesystem state and what is written to be executed in script
--
-- May be useful to check if Execute works correctly or too see if you really need to run this script
--
-- @
-- main ∷ IO ()
-- main = verify $ do
--   profile ...
--   profile ...
-- @
verify ∷ (Default s, Default t) ⇒ ProfileScript s t () → IO ()
verify s = do
  home ← getHomeDirectory
  let s' = infect home s
  (verified, failures) ← runWriterT (profile s')
  putStr "Verify… "
  if verified
    then putStrLn "OK"
    else putStrLn $ failures ++ "\nFail!"


profile ∷ Free (Profile (Free (Source (Free Files ())) ())) () → WriterT String IO Bool
profile = foldie (|&&|) (return True) f
 where
  f (Profile _ s _) = repo s


repo ∷ Free (Source (Free Files ())) () → WriterT String IO Bool
repo = foldie (|&&|) (return True) f
 where
  f s = do
    sourceExists ← io $ doesDirectoryExist (s ^. to)
    if sourceExists
      then files (s^.script)
      else do
        tellLn [indent 2, "Source ", s^.from, " → ", s^.to, " doesn't exist"]
        return False


files ∷ Free Files () → WriterT String IO Bool
files = foldie (|&&|) (return True) f
 where
  f (Message _ _) = return True
  f (RegisterAt _ dst _) = do
    repoExists ← io $ doesDirectoryExist dst
    unless repoExists $ tellLn [indent 4, "Repository link at ", dst, " does not exist"]
    return repoExists
  f (Link src dst _) = do
    src' ← io $ readSymbolicLink dst
    dstExists ← io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
    let correctLink = src == src' && dstExists
    unless correctLink $ tellLn [indent 4, "Link at ", dst, " is broken"]
    return correctLink
  f (Copy src dst _) = do
    src' ← io $ B.readFile src
    dst' ← io $ B.readFile dst
    let same = src' == dst'
    unless same $ tellLn [indent 4, "Files at ", src, " and ", dst, " are not copies"]
    return same
  f (Compile _ _ dst _) = do
    binaryExists ← io $ doesFileExist dst
    unless binaryExists $ tellLn [indent 4, "Compiled binary file at ", dst, " does not exist"]
    return binaryExists
  f (Template _ dst _ _) = io $ doesFileExist dst



(|&&|) ∷ Applicative m ⇒ m Bool → m Bool → m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io ∷ IO a → WriterT String IO a
io = liftIO


tellLn ∷ [String] → WriterT String IO ()
tellLn failure = tell . mconcat $ failure ++ ["\n"]


indent ∷ Int → String
indent n = replicate n ' '
