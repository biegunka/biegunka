{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Verify (verify) where

import Control.Applicative
import Control.Monad (unless)
import Data.Monoid (mconcat)

import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Posix.Files (readSymbolicLink)

import Biegunka.Control (Interpreter(..))
import Biegunka.Language.External (EL(..), Action(..))


-- | Verify interpreter
--
-- Compares current filesystem state and what is written to be executed in script
--
-- May be useful to check if Execute works correctly or too see if you really need to run this script
--
-- @
-- main :: IO ()
-- main = verify $ do
--   profile ...
--   profile ...
-- @
verify :: Interpreter
verify = I $ \_ s -> do
  (verified, failures) <- runWriterT (f s)
  putStr "Verify… "
  if verified
    then putStrLn "OK"
    else putStrLn $ failures ++ "\nFail!"


f :: [EL l () b] -> WriterT String IO Bool
f = foldr (|&&|) (return True) . map g


g :: EL l () b -> WriterT String IO Bool
g (EP _ _ _) = return True
g (ES _ u p _ _ _) = do
  sourceExists <- io $ doesDirectoryExist p
  unless sourceExists $ tellLn [indent 2, "Source ", u, " -> ", p, " doesn't exist"]
  return sourceExists
g (EF a _) = h a
 where
  h (Link src dst) = do
    src' <- io $ readSymbolicLink dst
    dstExists <- io $ (liftA2 (||) (doesFileExist src') (doesDirectoryExist src'))
    let correctLink = src == src' && dstExists
    unless correctLink $ tellLn [indent 4, "Link at ", dst, " is broken"]
    return correctLink
  h (Copy src dst) = do
    src' <- io $ B.readFile src
    dst' <- io $ B.readFile dst
    let same = src' == dst'
    unless same $ tellLn [indent 4, "Files at ", src, " and ", dst, " are not copies"]
    return same
  h (Template _ dst _) = io $ doesFileExist dst
  h (Shell {}) = return True
g (EW {}) = return True


(|&&|) :: Applicative m => m Bool -> m Bool -> m Bool
(|&&|) = liftA2 (&&)
infixr 3 |&&|


io :: IO a -> WriterT String IO a
io = liftIO


tellLn :: [String] -> WriterT String IO ()
tellLn failure = tell . mconcat $ failure ++ ["\n"]


indent :: Int -> String
indent n = replicate n ' '
