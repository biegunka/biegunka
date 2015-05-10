{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter, interpret, interpretOptimistically
    -- * Generic interpreters
  , pause, confirm
    -- * Auxiliary
  , expandHome
  ) where

import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad.Free (Free)
import           Data.Bool (bool)
import           Data.Char (toLower)
import           Data.Default.Class (Default(..))
import           Data.Function (fix)
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.Semigroup (Semigroup(..))
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import           Control.Biegunka.Language
import qualified Control.Biegunka.Log as Log
import           Control.Biegunka.Script (Script, Annotate, profiles, runScript)
import           Control.Biegunka.Script.Token (tokens)
import           Control.Biegunka.Settings
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen ((<//>), text, line)

{-# ANN module ("HLint: ignore Use join" :: String) #-}


-- | Abstract 'Interpreter' data type
newtype Interpreter = I
  (Settings () -> Free (Term Annotate 'Sources) () -> IO ExitCode -> IO ExitCode)

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
  :: (Settings () -> Free (Term Annotate 'Sources) () -> IO ExitCode -> IO ExitCode)
  -> Interpreter
interpret = I

-- | Construct 'Interpreter' optimistically
--
-- It is optimistic in a sense what it always calls the continuation, provided that
-- no exceptions were thrown
interpretOptimistically
  :: (Settings () -> Free (Term Annotate 'Sources) () -> IO ())
  -> Interpreter
interpretOptimistically f =
  interpret $ \c s k -> f c s >> k

-- | Run 'Interpreter'
runInterpreter :: Interpreter -> Settings () -> Free (Term Annotate 'Sources) () -> IO ExitCode
runInterpreter (I f) c s = f c s (return ExitSuccess)


-- | Entry point into the library
biegunka :: (Settings () -> Settings ()) -- ^ User defined settings
         -> Interpreter                  -- ^ Combined interpreters
         -> Script 'Sources ()           -- ^ Script to interpret
         -> IO ExitCode
biegunka (($ def) -> c) interpreter script = do
  rr <- views runRoot expandHome c
  br <- views biegunkaRoot expandHome c
  bracket Log.start Log.stop $ \queue -> do
    Log.write queue $
      Log.plain (text (info rr br c))
    let (annotatedScript, annotations) = runScript def (set runRoot rr def) tokens script
        settings = c
          & runRoot      .~ rr
          & biegunkaRoot .~ br
          & logger       .~ queue
          & targets      .~ views profiles Subset annotations
    runInterpreter interpreter settings annotatedScript
 where
  info rr br settings = unlines $
    [ "* Relative filepaths are deemed relative to " ++ rr
    , "* Data will be saved in "                     ++ br
    ] ++
    bool [] ["* Offline mode"] (has (mode._Offline) settings)


-- | Expand \"~\" at the start of the path
expandHome :: String -> IO String
expandHome ('~' : (splitUser -> (user, '/' : xs))) = getHome user <&> (</> xs)
expandHome ('~' : (splitUser -> (user, "")))       = getHome user
expandHome x = return x

-- | Break the path on the first '/'
splitUser :: String -> (String, String)
splitUser = break (== '/')

-- | Get home directory for user by name. If the name is empty return the value
-- of HOME environment variable
getHome :: String -> IO FilePath
getHome "" = Posix.getEnvDefault "HOME" ""
getHome user = fmap Posix.homeDirectory (Posix.getUserEntryForName user)

-- | Interpreter that just waits user to press any key
pause :: Interpreter
pause = interpretOptimistically $ \settings _ -> do
  Log.write (view logger settings) $
    Log.plain (text "Press any key to continue" <//> line)
  hSetBuffering stdin NoBuffering
  getChar
  hSetBuffering stdin LineBuffering

-- | Interpreter that awaits user confirmation
confirm :: Interpreter
confirm = interpret go
 where
  go settings _ ks = do
    k <- prompt (text "Proceed? [Y/n] ") -- choice of continuation is based on the user input
    k
   where
    prompt message = fix $ \loop -> do
      Log.write (view logger settings) $
        Log.plain message
      res <- getLine
      case map toLower res of
        "y" -> return ks
        ""  -> return ks
        "n" -> return (return (ExitFailure 1))
        _   -> loop
