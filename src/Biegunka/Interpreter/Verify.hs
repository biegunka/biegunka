{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Interpreter.Verify (verify) where

import Control.Applicative (Applicative, (<$>), liftA2)
import Control.Monad (unless)
import Data.Monoid (mconcat)

import           Control.Lens ((^.), view)
import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import           Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory)
import           System.Posix.Files (readSymbolicLink, fileMode, fileOwner, fileGroup, getFileStatus)
import           System.Posix.User (getGroupEntryForName, getUserEntryForName, groupID, userID)

import Biegunka.DSL
  ( ProfileScript
  , Layer(..), Command(..), Action(..)
  , action, from, to, script
  , foldie)
import Biegunka.Interpreter.State


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


profile ∷ Free (Command Profile (Free (Command Source (Free (Command Files ()) ())) ())) ()
        → WriterT String IO Bool
profile = foldie (|&&|) (return True) f
 where
  f ∷ Command Profile (Free (Command Source (Free (Command Files ()) ())) ()) a → WriterT String IO Bool
  f (P _ s _) = repo s
  f (W p _) = f p


repo ∷ Free (Command Source (Free (Command Files ()) ())) () → WriterT String IO Bool
repo = foldie (|&&|) (return True) f
 where
  f s = do
    sourceExists ← io $ doesDirectoryExist (s ^. to)
    if sourceExists
      then files (s^.script)
      else do
        tellLn [indent 2, "Source ", s^.from, " → ", s^.to, " doesn't exist"]
        return False


files ∷ Free (Command Files ()) () → WriterT String IO Bool
files = foldie (|&&|) (return True) (f . view action)
 where
  f (Message _) = return True
  f (RegisterAt _ dst) = do
    repoExists ← io $ doesDirectoryExist dst
    unless repoExists $ tellLn [indent 4, "Repository link at ", dst, " does not exist"]
    return repoExists
  f (Link src dst) = do
    src' ← io $ readSymbolicLink dst
    dstExists ← io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
    let correctLink = src == src' && dstExists
    unless correctLink $ tellLn [indent 4, "Link at ", dst, " is broken"]
    return correctLink
  f (Copy src dst) = do
    src' ← io $ B.readFile src
    dst' ← io $ B.readFile dst
    let same = src' == dst'
    unless same $ tellLn [indent 4, "Files at ", src, " and ", dst, " are not copies"]
    return same
  f (Compile _ _ dst) = do
    binaryExists ← io $ doesFileExist dst
    unless binaryExists $ tellLn [indent 4, "Compiled binary file at ", dst, " does not exist"]
    return binaryExists
  f (Mode fp m) = do
    m' ← io $ fileMode <$> getFileStatus fp
    let same = m == m'
    unless same $ tellLn [indent 4, "File mode of ", fp, " is not ", show m]
    return same
  f (Ownership fp u g) = do
    uid ← io $ userID <$> getUserEntryForName u
    gid ← io $ groupID <$> getGroupEntryForName g
    s ← io $ getFileStatus fp
    let uid' = fileOwner s
        gid' = fileGroup s
    let same = (uid,gid) == (uid',gid')
    unless same $ tellLn [indent 4, "File ownership of ", fp, " is not ", u, ":", g]
    return same
  f (Template _ dst _) = io $ doesFileExist dst



(|&&|) ∷ Applicative m ⇒ m Bool → m Bool → m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io ∷ IO a → WriterT String IO a
io = liftIO


tellLn ∷ [String] → WriterT String IO ()
tellLn failure = tell . mconcat $ failure ++ ["\n"]


indent ∷ Int → String
indent n = replicate n ' '
