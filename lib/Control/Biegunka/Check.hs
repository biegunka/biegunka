{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Check interpreter
module Control.Biegunka.Check (check) where

import Control.Applicative
import Control.Lens hiding (Action)
import Control.Monad
import Control.Monad.Catch (MonadCatch, catchIOError)
import Control.Monad.Free (Free(..), iterM)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Foldable (for_)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Posix.Files
  (fileOwner, getSymbolicLinkStatus, readSymbolicLink)
import System.Posix.User
import System.Posix.Types (UserID)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Control.Biegunka.Action (verifyAppliedPatch, verifyCopy)
import Control.Biegunka.Biegunka (Interpreter(..), interpret)
import Control.Biegunka.Settings
  (logger, ColorScheme(..), colors, sourceColor, srcColor, dstColor)
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
  | CannotDetermineOwnerOf FilePath
    deriving (Show, Read)


-- | Check interpreter
check :: Interpreter
check = interpret $ \settings terms -> do
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
    checkActionResult action
    checkOwnage aaUser action
    next
  TM _ next -> next

-- | Check that result of term execution is what's expected
checkActionResult :: Action -> WriterT [CheckFailure] IO ()
checkActionResult = \case
  Link file target -> do
    file' <- io $ readSymbolicLink target
    dfe   <- io $ doesFileExist file'
    dde   <- io $ doesDirectoryExist file'
    unless (file == file' && (dfe || dde)) $
      failure (target `NotALinkTo` file)
   `catchedIOError`
    tell [target `NotALinkTo` file]
  Copy file target spec -> do
    copy <- io $ verifyCopy file target spec
    unless copy $
      failure (target `NotACopyOf` file)
   `catchedIOError`
    failure (target `NotACopyOf` file)
  Template file target _ -> do
    exists <- io $ doesFileExist target
    unless exists $
      failure (target `NotATemplateInstanceOf` file)
   `catchedIOError`
    failure (target `NotATemplateInstanceOf` file)
  Patch patch target spec -> do
    verified <- io $ verifyAppliedPatch patch target spec
    unless verified $
      failure ((target `NotPatchedWith` patch) spec)
   `catchedIOError`
    failure ((target `NotPatchedWith` patch) spec)
  Command _ _ ->
    return ()

-- | Check that stuff is owned by correct user
checkOwnage :: Maybe UserW -> Action -> WriterT [CheckFailure] IO ()
checkOwnage auser = \case
  Link _ target       -> ownage target
  Copy _ target _     -> ownage target
  Template _ target _ -> ownage target
  Patch _ target _    -> ownage target
  Command _ _         -> return ()
 where
  ownage :: FilePath -> WriterT [CheckFailure] IO ()
  ownage target = do
    owning <- io $ compareUsers auser target
    for_ owning $ \user ->
      failure $ user `IncorrectOwnerOf` target
   `catchedIOError`
    failure (CannotDetermineOwnerOf target)

compareUsers :: Maybe UserW -> FilePath -> IO (Maybe String)
compareUsers mu path = do
  desiredUserID <- user mu
  realUserID    <- fileOwner <$> getSymbolicLinkStatus path
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
  target `NotACopyOf` file ->
        (scheme^.dstColor) (text target)
    </> "is not a copy of"
    </> (scheme^.srcColor) (text file)
  target `NotALinkTo` file ->
        (scheme^.dstColor) (text target)
    </> "link to"
    </> (scheme^.srcColor) (text file)
    </> "is broken"
  target `NotATemplateInstanceOf` file ->
        (scheme^.dstColor) (text target)
    </> "is not a templated copy of"
    </> (scheme^.srcColor) (text file)
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
  CannotDetermineOwnerOf target ->
        "cannot determine who owns"
    </> (scheme^.dstColor) (text target)

catchedIOError :: MonadCatch m => m a -> m a -> m a
catchedIOError ma ma' = ma `catchIOError` \_ -> ma'

io :: MonadIO m => IO a -> m a
io = liftIO

failure :: MonadWriter [w] m => w -> m ()
failure m = tell [m]
