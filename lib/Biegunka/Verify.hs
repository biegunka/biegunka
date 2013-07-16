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
import           Data.Copointed (copoint)
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.IO.Error (catchIOError)
import           System.Posix.Files (readSymbolicLink)
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Biegunka.Action (verifyAppliedPatch, verifyCopy)
import Biegunka.Control
  ( Interpreter(..), Settings, interpret, logger
  , ColorScheme(..), colors
  , sourceColor, srcColor, dstColor
  )
import Biegunka.Language
import Biegunka.Script (Annotate(..))


-- | Check interpreter
check :: Interpreter
check = interpret $ \c (s, _) -> do
  failures <- execWriterT (verification c s)
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
verification :: Settings () -> Free (Term Annotate s) () -> WriterT [Doc] IO ()
verification c (Free t) = do
  r <- liftIO (correct t `mplus` return False)
  if r then case t of
    TS _ _ i _ -> verification c i
    _ -> return ()
  else
    traverse_ (tell . (:[])) (termDescription <$> log (c^.colors) t)
  verification c (copoint t)
 where
verification _ (Pure ()) = return ()

-- | Check single instruction correctness
correct :: Term Annotate s a -> IO Bool
correct il = case il of
  TS _ (Source { spath }) _ _ -> doesDirectoryExist spath
  TA _ a _ -> case a of
    Link s d -> do
      s' <- readSymbolicLink d
      dfe <- doesFileExist s'
      dde <- doesDirectoryExist s'
      return $ s == s' && (dfe || dde)
    Copy source destination spec -> do
      verifyCopy source destination spec
    Template _ d _ ->
      doesFileExist d
    Patch patch root spec ->
      verifyAppliedPatch patch root spec
    Command _ _ ->
      return True
   `catchIOError`
      \_ -> return False
  TM _ _ -> return True


-- | Describe current action and host where it happens
termDescription :: Doc -> Doc
termDescription d = let host = "[localhost]" :: String in nest (length host) $ text host </> d

-- | Log message on failure
log :: ColorScheme -> Term Annotate s a -> Maybe Doc
log sc il = nest 1 <$> case il of
  TS _ (Source t u d _) _ _  ->
    Just $ text t </> "source" </> parens ((sc^.sourceColor) (text u)) </> "does not exist at" </> (sc^.dstColor) (text d)
  TA (AA { aaURI }) a _ -> annotation (text aaURI) <$> case a of
    Link s d -> Just $
          (sc^.dstColor) (text d)
      </> "link to"
      </> (sc^.srcColor) (text s)
      </> "is broken"
    Copy s d _ -> Just $
          (sc^.dstColor) (text d)
      </> "is not a copy of"
      </> (sc^.srcColor) (text s)
    Template s d _ -> Just $
          (sc^.dstColor) (text d)
      </> "is not a templated copy of"
      </> (sc^.srcColor) (text s)
    Command _ _ -> Nothing
    Patch patch patchRoot PatchSpec { reversely } -> Just $
          (sc^.srcColor) (text patch)
      </> "is not correctly"
      </> (if reversely then parens "reversely" </> "applied" else "applied")
      </> "to"
      </> (sc^.dstColor) (text patchRoot)
  TM _ _ -> Nothing
 where
  -- | Annotate action description with source name
  annotation :: Doc -> Doc -> Doc
  annotation t doc = parens ((sc^.sourceColor) t) L.<$> doc
