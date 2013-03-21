{-# LANGUAGE OverloadedStrings #-}
-- | Verification interpreter
module Biegunka.Verify (verify) where

import Control.Applicative
import Control.Monad (mplus, unless)
import Data.List (foldl')
import Prelude hiding (log)

import           Control.Lens
import           Control.Monad.Writer (WriterT, runWriterT, tell)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Posix.Files (readSymbolicLink)
import           Text.PrettyPrint.Free
import           System.Console.Terminfo.PrettyPrint

import Biegunka.Control (Interpreter(..), logger)
import Biegunka.Language (IL(..), A(..))


-- | Verification interpreter
--
-- Compares current filesystem layout and what script says it should be line by line.
-- Outputs errors it find, otherwise prints OK. Is useful to check execution correctness.
verify :: Interpreter
verify = I $ \c s -> do
  (verified, failures) <- runWriterT (verification s)
  view logger c $ (if verified then green "OK" else vcat failures) <> line


-- | Check layout correctness instruction by instruction creating failures log line by line
verification :: [IL] -> WriterT [TermDoc] IO Bool
verification = foldl' (\a -> liftA2 (&&) a . go) (return True)
 where
  go i = case log i of
    Just l  -> liftIO (correct i `mplus` return False) >>= \c -> unless c (tell [l]) >> return c
    Nothing -> return True

-- | Check single instruction correctness
correct :: IL -> IO Bool
correct (IS p _ _ _ _ _) = doesDirectoryExist p
correct (IA (Link s d) _ _ _) = do
  s' <- readSymbolicLink d
  dfe <- doesFileExist s'
  dde <- doesDirectoryExist s'
  return $ s == s' && (dfe || dde)
correct (IA (Copy s d) _ _ _) = do
  s' <- B.readFile s
  d' <- B.readFile d
  return $ s' == d'
correct (IA (Template _ d _) _ _ _) = doesFileExist d
correct _ = return True

-- | Log message on failure
log :: IL -> Maybe TermDoc
log (IS p t _ _ _ u) =
  Just $ text t </> "source" </> parens (cyan (text u)) </> "does not exist at" </> magenta (text p)
log (IA (Link src dst) _ _ _) =
  Just . indent 2 $ yellow (text dst) </> "link to" </> magenta (text src) </> "is broken"
log (IA (Copy src dst) _ _ _) = do
  Just . indent 2 $ yellow (text dst) </> "is not a copy of" </> magenta (text src)
log (IA (Template src dst _) _ _ _) =
  Just . indent 2 $ yellow (text dst) </> "is not a templated copy teplates of" </> magenta (text src)
log _ = Nothing
