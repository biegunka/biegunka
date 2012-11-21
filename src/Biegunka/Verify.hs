{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Verify (verify) where

import Control.Applicative (Applicative, (<$>), liftA2)
import Control.Monad (unless)
import Data.Monoid (mconcat)

import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink, fileMode, fileOwner, fileGroup, getFileStatus)
import           System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)

import Biegunka.DSL (Script, Layer(..), Command(..), Action(..), foldie)
import Biegunka.Flatten
import Biegunka.State


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
verify ∷ Script Profile → IO ()
verify s = do
  home ← getHomeDirectory
  (verified, failures) ← runWriterT . f . infect home . flatten $ s
  putStr "Verify… "
  if verified
    then putStrLn "OK"
    else putStrLn $ failures ++ "\nFail!"


f ∷ Free (Command l ()) () → WriterT String IO Bool
f = foldie (|&&|) (return True) g


g ∷ Command l () (Free (Command l ()) ()) → WriterT String IO Bool
g (P _ _ _) = return True
g (S u p _ _ _) = do
  sourceExists ← io $ doesDirectoryExist p
  unless sourceExists $ tellLn [indent 2, "Source ", u, " → ", p, " doesn't exist"]
  return sourceExists
g (S' {}) = return True
g (F a _) = h a
 where
  h (Message _) = return True
  h (RegisterAt _ dst) = do
    repoExists ← io $ doesDirectoryExist dst
    unless repoExists $ tellLn [indent 4, "Repository link at ", dst, " does not exist"]
    return repoExists
  h (Link src dst) = do
    src' ← io $ readSymbolicLink dst
    dstExists ← io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
    let correctLink = src == src' && dstExists
    unless correctLink $ tellLn [indent 4, "Link at ", dst, " is broken"]
    return correctLink
  h (Copy src dst) = do
    src' ← io $ B.readFile src
    dst' ← io $ B.readFile dst
    let same = src' == dst'
    unless same $ tellLn [indent 4, "Files at ", src, " and ", dst, " are not copies"]
    return same
  h (Compile _ _ dst) = do
    binaryExists ← io $ doesFileExist dst
    unless binaryExists $ tellLn [indent 4, "Compiled binary file at ", dst, " does not exist"]
    return binaryExists
  h (Mode fp mode) = do
    mode' ← io $ fileMode <$> getFileStatus fp
    let same = mode == mode'
    unless same $ tellLn [indent 4, "File mode of ", fp, " is not ", show mode]
    return same
  h (Ownership fp user group) = do
    uid ← io $ userID <$> getUserEntryForName user
    gid ← io $ groupID <$> getGroupEntryForName group
    s ← io $ getFileStatus fp
    let uid' = fileOwner s
        gid' = fileGroup s
    let same = (uid,gid) == (uid',gid')
    unless same $ tellLn [indent 4, "File ownership of ", fp, " is not ", user, ":", group]
    return same
  h (Template _ dst _) = io $ doesFileExist dst
g (W {}) = return True


(|&&|) ∷ Applicative m ⇒ m Bool → m Bool → m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io ∷ IO a → WriterT String IO a
io = liftIO


tellLn ∷ [String] → WriterT String IO ()
tellLn failure = tell . mconcat $ failure ++ ["\n"]


indent ∷ Int → String
indent n = replicate n ' '
