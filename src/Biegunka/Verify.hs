{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import Biegunka.Transform (simplified)


-- | Verification interpreter
--
-- Compares current filesystem layout and what script says it should be line by line.
-- Outputs errors it find, otherwise prints OK. Is useful to check execution correctness.
verify :: Interpreter
verify = I $ \c (simplified -> s) -> do
  (verified, failures) <- runWriterT (verification s)
  view logger c $
    text "Verification:" <> line <>
    (if verified then green "OK" else vcat failures) <> line


-- | Check layout correctness instruction by instruction creating failures log line by line
verification :: [IL] -> WriterT [TermDoc] IO Bool
verification = foldl' (\a -> liftA2 (&&) a . go) (return True)
 where
  go i = case log i of
    Just l  -> liftIO (correct i `mplus` return False) >>= \c -> unless c (tell [describe l]) >> return c
    Nothing -> return True

-- | Check single instruction correctness
correct :: IL -> IO Bool
correct il = case il of
  IS p _ _ _ _ -> doesDirectoryExist p
  IA a _ _ _ _ -> case a of
    Link s d -> do
      s' <- readSymbolicLink d
      dfe <- doesFileExist s'
      dde <- doesDirectoryExist s'
      return $ s == s' && (dfe || dde)
    Copy s d -> do
      s' <- B.readFile s
      d' <- B.readFile d
      return $ s' == d'
    Template _ d _ -> doesFileExist d
    _ -> return True
  _ -> return True



-- | Describe current action and host where it happens
describe :: TermDoc -> TermDoc
describe d = let host = "[localhost]" :: String in nest (length host) $ text host </> d


-- | Log message on failure
log :: IL -> Maybe TermDoc
log il = nest 1 <$> case il of
  IS p t _ _ u ->
    Just $ text t </> "source" </> parens (cyan (text u)) </> "does not exist at" </> magenta (text p)
  IA a _ _ _ n -> annotation (text n) <$> case a of
    Link s d ->
      Just $ yellow (text d) </> "link to" </> magenta (text s) </> "is broken"
    Copy s d ->
      Just $ yellow (text d) </> "is not a copy of" </> magenta (text s)
    Template s d _ ->
      Just $ yellow (text d) </> "is not a templated copy of" </> magenta (text s)
    _ -> Nothing
  _ -> Nothing
 where
  -- | Annotate action description with source name
  annotation :: TermDoc -> TermDoc -> TermDoc
  annotation t doc = parens (cyan t) </> doc
