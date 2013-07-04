{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Check interpreter
module Biegunka.Verify (check, verify) where

import Control.Applicative
import Control.Monad (mplus)
import Data.Foldable (traverse_)
import Prelude hiding (log)

import           Control.Lens
import           Control.Monad.Free (Free(..))
import           Control.Monad.Writer (WriterT, execWriterT, tell)
import           Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Lazy as B
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Posix.Files (readSymbolicLink)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Biegunka.Control (Interpreter(..), interpret, logger)
import Biegunka.Language (Term(..), S(..), A(..), peek)
import Biegunka.Script (Annotate(..))


-- | Check interpreter
check :: Interpreter
check = interpret $ \c s -> do
  failures <- execWriterT (verification s)
  view logger c $
       text "Verification: "
    <> case failures of
      [] -> green "OK"
      _  -> line <> vcat failures
    <> line

-- | Check interpreter
verify :: Interpreter
verify = check
{-# DEPRECATED verify "Please, use `check'" #-}

-- | Check layout correctness instruction by instruction creating failures log line by line
verification :: Free (Term Annotate s) () -> WriterT [Doc] IO ()
verification (Free c) = do
  r <- liftIO (correct c `mplus` return False)
  if r then case c of
    EP _ _ i _ -> verification i
    ES _ _ i _ -> verification i
    _ -> return ()
  else
    traverse_ (tell . (:[])) (describe <$> log c)
  verification (peek c)
 where
verification (Pure ()) = return ()

-- | Check single instruction correctness
correct :: Term Annotate s a -> IO Bool
correct il = case il of
  ES _ (S { spath }) _ _ -> doesDirectoryExist spath
  EA _ a _ -> case a of
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
describe :: Doc -> Doc
describe d = let host = "[localhost]" :: String in nest (length host) $ text host </> d

-- | Log message on failure
log :: Term Annotate s a -> Maybe Doc
log il = nest 1 <$> case il of
  ES _ (S t u d _) _ _  ->
    Just $ text t </> "source" </> parens (cyan (text u)) </> "does not exist at" </> magenta (text d)
  EA (SAA { saaURI }) a _ -> annotation (text saaURI) <$> case a of
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
  annotation :: Doc -> Doc -> Doc
  annotation t doc = parens (cyan t) L.<$> doc
