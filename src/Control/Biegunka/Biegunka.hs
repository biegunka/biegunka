{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka
  , Interpreter(..)
  , optimistically
    -- * Generic interpreters
  , pause
  , confirm
    -- * Auxiliary
  , expandHome
  ) where

import           Control.Exception (catchJust)
import           Control.Lens
import           Control.Monad (guard)
import           Control.Monad.Free (Free)
import           Data.Bool (bool)
import           Data.Char (toLower)
import           Data.Function (fix)
import qualified Data.List as List
#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif
import           Data.Semigroup (Semigroup(..))
import           Data.Version (showVersion)
import           System.Exit (ExitCode(..))
import           System.FilePath ((</>))
import qualified System.Posix as Posix

import           Control.Biegunka.Language
import qualified Control.Biegunka.Logger as Logger
import           Control.Biegunka.Script
  (Script, Annotate, defaultMAnnotations, defaultAnnotations, evalScript)
import           Control.Biegunka.Settings
import qualified System.IO as IO
import qualified System.IO.Error as IO

import qualified Git_biegunka as Git
import           Paths_biegunka (version)


-- | 'Interpreter' data type.
--
-- Given a 'Settings', a script, and a continuation restulting in an 'ExitCode'
-- does something resulting in an 'ExitCode'.
newtype Interpreter = I
  (Settings -> Free (Term Annotate 'Sources) () -> IO ExitCode -> IO ExitCode)

-- | The composition of two 'Interpreter's is the 'Interpreter' in which the
-- second operand is the continuation of the first.
instance Semigroup Interpreter where
  I f <> I g = I $ \c s k -> f c s (g c s k)

-- | Empty 'Interpreter' is the 'Interpreter' that does nothing and simply
-- calls the continuation.
instance Monoid Interpreter where
  mempty  = I $ \_ _ k -> k
  mappend = (<>)

-- | Optimistic 'Interpreter' always calls the continuation, assuming that
-- no exceptions were thrown.
optimistically
  :: (Settings -> Free (Term Annotate 'Sources) () -> IO ())
  -> Interpreter
optimistically f =
  I (\c s k -> f c s >> k)

-- | Entry point into the library
biegunka :: (Settings -> Settings ) -- ^ User defined settings
         -> Interpreter             -- ^ Combined interpreters
         -> Script 'Sources ()      -- ^ Script to interpret
         -> IO ExitCode
biegunka (($ defaultSettings) -> c) (I interpret) script = do
  rr <- views runRoot expandHome c
  br <- views biegunkaRoot expandHome c
  Logger.with $ \l -> do
    Logger.write IO.stdout l (info rr br)
    let annotatedScript = evalScript defaultMAnnotations (set runRoot rr defaultAnnotations) script
        c' = c
          & runRoot      .~ rr
          & biegunkaRoot .~ br
          & _logger      ?~ l
    interpret c' annotatedScript (return ExitSuccess)
 where
  info rr br = List.intercalate "\n" $
    [ "* Library version: " ++ showVersion version ++ "-" ++ Git.hash
    , "* Relative filepaths are deemed relative to " ++ rr
    , "* Data will be saved in "                     ++ br
    ] ++
    bool [] ["* Offline mode"] (has (mode._Offline) c)


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
pause = optimistically $ \c _ -> do
  Logger.write IO.stdout c "Press any key to continue\n"
  IO.hSetBuffering IO.stdin IO.NoBuffering
  getChar
  IO.hSetBuffering IO.stdin IO.LineBuffering

-- | Interpreter that awaits user confirmation
confirm :: Interpreter
confirm = I go
 where
  go c _ ks =
    catchJust
      (guard . IO.isEOFError)
      (do k <- prompt "Proceed? [Y/n] "
          k)
      (\_ ->
         do Logger.write IO.stdout c "\n"
            return ExitSuccess)
   where
    prompt message = fix $ \loop -> do
      Logger.write IO.stdout c message
      res <- getLine
      case map toLower res of
        "y" -> return ks
        ""  -> return ks
        "n" -> return (return (ExitFailure 1))
        _   -> loop
{-# ANN confirm ("HLint: ignore Use join" :: String) #-}
