{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Check interpreter
module Control.Biegunka.Check (check) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadCatch, catchIOError)
import Control.Monad.Free (Free(..), iterM)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable (for_)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Posix.Files (fileOwner, getFileStatus, readSymbolicLink)
import System.Posix.User
import System.Posix.Types (UserID)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Control.Biegunka.Action (verifyAppliedPatch, verifyCopy)
import Control.Biegunka.Settings
  ( Interpreter(..), interpret, logger
  , ColorScheme(..), colors
  , sourceColor, srcColor, dstColor
  )
import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Various failures that may happen before/after
-- biegunka script run.
data CheckFailure =
    NonExistingSource URI FilePath
  | FilePath `NotACopyOf` FilePath
  | FilePath `NotALinkTo` FilePath
  | FilePath `NotATemplateInstanceOf` FilePath
  | NotPatchedWith FilePath FilePath PatchSpec
  | String `IncorrectOwnerOf` FilePath
    deriving (Show, Read)


-- | Check interpreter
check :: Interpreter
check = interpret $ \settings terms _ -> do
  let document = documentCheckFailure (settings^.colors)
  failures <- execWriterT (verification terms)
  view logger settings $
       text "Verification: "
    <> case failures of
      [] -> green "OK"
      _  -> line <> vcat (map document failures)
    <> line

-- | Check and document all failures, assuming script was ran
verification :: Free (Term Annotate s) () -> WriterT [CheckFailure] IO ()
verification = iterM $ \case
  TS _ (Source { spath, suri }) inner next -> do
    exists <- io $ doesDirectoryExist spath
    case exists of
      True  -> verification inner
      False -> tell [NonExistingSource spath suri]
    next
  TA (AA { aaUser }) action next -> do
    case action of
      Link source target -> do
        source' <- io $ readSymbolicLink target
        dfe     <- io $ doesFileExist source'
        dde     <- io $ doesDirectoryExist source'
        unless (source == source' && (dfe || dde)) $
          failure (target `NotALinkTo` source)
        checkOwnage aaUser target
       `catchedIOError`
        tell [target `NotALinkTo` source]
      Copy source target spec -> do
        copy <- io $ verifyCopy source target spec
        unless copy $
          failure (target `NotACopyOf` source)
        checkOwnage aaUser target
       `catchedIOError`
        failure (target `NotACopyOf` source)
      Template source target _ -> do
        exists <- io $ doesFileExist target
        unless exists $
          failure (target `NotATemplateInstanceOf` source)
        checkOwnage aaUser target
       `catchedIOError`
        failure (target `NotATemplateInstanceOf` source)
      Patch patch target spec -> do
        verified <- io $ verifyAppliedPatch patch target spec
        unless verified $
          failure ((target `NotPatchedWith` patch) spec)
        checkOwnage aaUser target
       `catchedIOError`
        failure ((target `NotPatchedWith` patch) spec)
      Command _ _ ->
        return ()
    next
  TM _ next -> next


checkOwnage :: Maybe UserW -> FilePath -> WriterT [CheckFailure] IO ()
checkOwnage auser target = do
  owning  <- io $ compareUsers auser target
  for_ owning $ \user ->
    failure $ user `IncorrectOwnerOf` target

compareUsers :: Maybe UserW -> FilePath -> IO (Maybe String)
compareUsers mu path = do
  desiredUserID <- user mu
  realUserID    <- fileOwner <$> getFileStatus path
  case desiredUserID == realUserID of
    True  -> return Nothing
    False -> Just . userName <$> getUserEntryForID realUserID
 where
  user :: Maybe UserW -> IO UserID
  user (Just (UserW (UserID i))) = return i
  user (Just (UserW (Username s))) = userID <$> getUserEntryForName s
  user Nothing = getEffectiveUserID


documentCheckFailure :: ColorScheme -> CheckFailure -> Doc
documentCheckFailure scheme = \case
  NonExistingSource uri target ->
        "source"
    </> parens ((scheme^.sourceColor) (text uri))
    </> "does not exist at"
    </> (scheme^.dstColor) (text target)
  target `NotACopyOf` source ->
        (scheme^.dstColor) (text target)
    </> "is not a copy of"
    </> (scheme^.srcColor) (text source)
  target `NotALinkTo` source ->
        (scheme^.dstColor) (text target)
    </> "link to"
    </> (scheme^.srcColor) (text source)
    </> "is broken"
  target `NotATemplateInstanceOf` source ->
        (scheme^.dstColor) (text target)
    </> "is not a templated copy of"
    </> (scheme^.srcColor) (text source)
  NotPatchedWith target patch PatchSpec { reversely } ->
        (scheme^.srcColor) (text patch)
    </> "is not correctly"
    </> (if reversely then parens "reversely" </> "applied" else "applied")
    </> "to"
    </> (scheme^.dstColor) (text target)
  name `IncorrectOwnerOf` target ->
        (scheme^.dstColor) (text target)
    </> "is incorrectly owned by"
    </> (scheme^.sourceColor) (text name)

catchedIOError :: MonadCatch m => m a -> m a -> m a
catchedIOError ma ma' = ma `catchIOError` \_ -> ma'

io :: MonadIO m => IO a -> m a
io = liftIO

failure :: MonadWriter [w] m => w -> m ()
failure m = tell [m]
