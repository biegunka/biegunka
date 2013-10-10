{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Control.Biegunka.Biegunka
  ( -- * Wrap/unwrap biegunka interpreters
    biegunka, Interpreter(..), interpret
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
import           Data.Semigroup (Semigroup(..), Monoid(..))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified System.Directory as D
import           System.FilePath ((</>))

import           Control.Biegunka.Language
import qualified Control.Biegunka.Logger as Logger
import           Control.Biegunka.Script
  (HasRoot(root), Script, Annotate, app, profiles, runScript)
import           Control.Biegunka.Settings
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen ((<//>), text, line)


-- | Interpreter newtype. Takes 'Controls', 'Script' and performs some 'IO'
newtype Interpreter = I
  { unInterpreter
      :: Settings ()
      -> Free (Term Annotate Sources) ()
      -> IO ()
      -> IO ()
  }

-- | Default 'Interpreter' does nothing
instance Default Interpreter where
  def = I $ \_ _ k -> k

-- | Two 'Interpreter's combined take the same 'Script' and do things one after another
instance Semigroup Interpreter where
  I f <> I g = I $ \c s k -> f c s (g c s k)

-- | Combination of the 'Default' and 'Semigroup' instances
instance Monoid Interpreter where
  mempty  = def
  mappend = (<>)

-- | Interpreter that calls its continuation after interpretation
interpret
  :: (Settings () -> Free (Term Annotate Sources) () -> IO ())
  -> Interpreter
interpret f = I (\c s k -> f c s >> k)

runInterpreter :: Interpreter -> Settings () -> Free (Term Annotate 'Sources) () -> IO ()
runInterpreter (I f) c s = f c s (return ())


-- | Common 'Interpreter's 'Controls' wrapper
biegunka :: (Settings () -> Settings ()) -- ^ User defined settings
         -> Interpreter                 -- ^ Combined interpreters
         -> Script Sources ()           -- ^ Script to interpret
         -> IO ()
biegunka (($ def) -> c) interpreter script = do
  appRoot <- c^.root.to expandHome
  dataDir <- c^.appData.to expandHome
  T.putStrLn $ info appRoot dataDir c
  bracket Logger.start Logger.stop $ \logQueue -> do
    let (annotatedScript, annotations) = runScript def (def & app .~ appRoot) script
        settings = c
          & root    .~ appRoot
          & appData .~ dataDir
          & logger  .~ Logger.write logQueue
          & targets .~ annotations^.profiles.to Subset
    runInterpreter interpreter settings annotatedScript
 where
  info appRoot dataDir settings = T.unlines $
    [ "* Relative filepaths are deemed relative to " `mappend` T.pack appRoot
    , "* Data will be saved in "                     `mappend` T.pack dataDir
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
pause = interpret $ \c _ -> view logger c (text "Press any key to continue" <//> line) >> getch
 where
  getch = do
    hSetBuffering stdin NoBuffering
    _ <- getChar
    hSetBuffering stdin LineBuffering

-- | Interpreter that awaits user confirmation
confirm :: Interpreter
confirm = I $ \c _ k -> do
  r <- prompt (view logger c) (text "Proceed? [y/n] ")
  case r of
    True  -> k
    False -> return ()
 where
  prompt l m = do
    l m
    r <- getLine
    case map toLower r of
      "y" -> return True
      "n" -> return False
      _   -> prompt l m
