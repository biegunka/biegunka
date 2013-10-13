{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter, interpret, optimisticallyInterpret
    -- * Generic interpreters
  , pause, confirm
    -- * Auxiliary
  , expandHome
  ) where

import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad.Free (Free)
import           Data.Char (toLower)
import           Data.Default
import           Data.Function (fix)
import           Data.Semigroup (Semigroup(..), Monoid(..))
import qualified System.Directory as D
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))

import           Control.Biegunka.Language
import qualified Control.Biegunka.Log as Log
import           Control.Biegunka.Script
  (HasRoot(root), Script, Annotate, app, profiles, runScript)
import           Control.Biegunka.Settings
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen ((<//>), text, line)


-- | Abstract 'Interpreter' data type
newtype Interpreter = I
  (Settings () -> Free (Term Annotate Sources) () -> IO ExitCode -> IO ExitCode)

-- | Default 'Interpreter' does nothing
instance Default Interpreter where
  def = I $ \_ _ k -> k

-- | Two 'Interpreter's combined take the same script and do things with it one after another
instance Semigroup Interpreter where
  I f <> I g = I $ \c s k -> f c s (g c s k)

-- | Combination of the 'Default' and 'Semigroup' instances
instance Monoid Interpreter where
  mempty  = def
  mappend = (<>)

-- | Construct 'Interpreter'
--
-- Provides 'Settings', script and continuation, which is supposed to be called
-- on success to interpreter
interpret
  :: (Settings () -> Free (Term Annotate Sources) () -> IO ExitCode -> IO ExitCode)
  -> Interpreter
interpret = I

-- | Construct 'Interpreter' optimistically
--
-- It is optimistic in a sense what it always calls the continuation, provided that
-- no exceptions were thrown
optimisticallyInterpret
  :: (Settings () -> Free (Term Annotate Sources) () -> IO ())
  -> Interpreter
optimisticallyInterpret f =
  interpret $ \c s k -> f c s >> k

-- | Run 'Interpreter'
runInterpreter :: Interpreter -> Settings () -> Free (Term Annotate 'Sources) () -> IO ExitCode
runInterpreter (I f) c s = f c s (return ExitSuccess)


-- | Entry point into the library
biegunka :: (Settings () -> Settings ()) -- ^ User defined settings
         -> Interpreter                 -- ^ Combined interpreters
         -> Script Sources ()           -- ^ Script to interpret
         -> IO ExitCode
biegunka (($ def) -> c) interpreter script = do
  appRoot <- c^.root.to expandHome
  dataDir <- c^.appData.to expandHome
  bracket Log.start Log.stop $ \queue -> do
    Log.write queue $
      Log.plain (text (info appRoot dataDir c))
    let (annotatedScript, annotations) = runScript def (def & app .~ appRoot) script
        settings = c
          & root    .~ appRoot
          & appData .~ dataDir
          & logger  .~ queue
          & targets .~ annotations^.profiles.to Subset
    runInterpreter interpreter settings annotatedScript
 where
  info appRoot dataDir settings = unlines $
    [ "* Relative filepaths are deemed relative to " ++ appRoot
    , "* Data will be saved in "                     ++ dataDir
    ] ++
    maybe [] (\_ -> return "* Offline mode") (settings ^? mode._Offline)


-- | Expand \"~\" at the start of pattern
expandHome :: String -> IO String
expandHome pat =
  case pat of
    "~"        -> D.getHomeDirectory
    '~':'/':xs -> do
      home <- D.getHomeDirectory
      return (home </> xs)
    _          -> return pat


-- | Interpreter that just waits user to press any key
pause :: Interpreter
pause = optimisticallyInterpret $ \settings _ -> do
  Log.write (settings^.logger) $
    Log.plain (text "Press any key to continue" <//> line)
  hSetBuffering stdin NoBuffering
  getChar
  hSetBuffering stdin LineBuffering

-- | Interpreter that awaits user confirmation
confirm :: Interpreter
confirm = interpret go
 where
  go settings _ ks = do
    k <- prompt (text "Proceed? [y/n] ") -- choice of continuation is based on the user input
    k
   where
    prompt message = fix $ \loop -> do
      Log.write (settings^.logger) $
        Log.plain message
      res <- getLine
      case map toLower res of
        "y" -> return ks
        "n" -> return (return (ExitFailure 1))
        _   -> loop
