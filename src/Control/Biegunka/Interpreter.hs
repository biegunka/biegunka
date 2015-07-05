{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
module Control.Biegunka.Interpreter
  ( Interpreter(..)
  , optimistically
  , pause
  , confirm
  , changes
  ) where

import           Control.Lens
import           Control.Exception (catchJust)
import           Control.Monad (guard)
import           Data.Char (toLower)
import           Data.Function (fix, on)
import qualified Data.List as List
import           Data.Semigroup (Semigroup(..))
import           System.Exit (ExitCode(..))
import qualified System.IO as IO
import qualified System.IO.Error as IO
import           Text.Printf (printf)

import           Control.Biegunka.Language (Term, Scope(Sources))
import qualified Control.Biegunka.Logger as Logger
import qualified Control.Biegunka.Namespace as Ns
import           Control.Biegunka.Script (Annotate)
import           Control.Biegunka.Settings (Settings)


-- | 'Interpreter' data type.
--
-- Given a 'Settings', a script, and a continuation restulting in an 'ExitCode'
-- does something resulting in an 'ExitCode'.
newtype Interpreter = I
  (Settings -> Term Annotate 'Sources () -> IO ExitCode -> IO ExitCode)

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
  :: (Settings -> Term Annotate 'Sources () -> IO ())
  -> Interpreter
optimistically f =
  I (\c s k -> f c s >> k)


-- | Waits for user to press any key.
pause :: Interpreter
pause = optimistically $ \c _ -> do
  Logger.write IO.stdout c "Press any key to continue\n"
  IO.hSetBuffering IO.stdin IO.NoBuffering
  getChar
  IO.hSetBuffering IO.stdin IO.LineBuffering

-- | Wait for user to confirm.
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

-- | Show the changes that will be made when the script is run.
changes :: Interpreter
changes = optimistically $ \settings s ->
  Ns.withDb settings $ \db ->
    Logger.write IO.stdout settings (runChanges db (Ns.fromScript s))
 where
  -- | Describe changes which will happen after the run
  runChanges :: Ns.Db -> Ns.Namespaces -> String
  runChanges db gs = unlines $ "" : concatMap about
    [ ("deleted files",    red,    map Ns.filePath df)
    , ("deleted sources",  red,    map Ns.sourcePath ds)
    , ("modified files",   yellow, map Ns.filePath mf)
    , ("modified sources", yellow, map Ns.sourcePath ms)
    , ("added files",      green,  map Ns.filePath nf)
    , ("added sources",    green,  map Ns.sourcePath ns)
    ] ++ [""]
   where
    about (msg, color, xs) = case length xs of
      0 -> []
      n -> printf "%s (%d):" msg n : map (\x -> "  " ++ color ++ x ++ reset) xs
    (df, mf, nf) = makeDiff Ns.filePath (Ns.files (view Ns.namespaces db)) (Ns.files gs)
    (ds, ms, ns) = makeDiff Ns.sourcePath (Ns.sources (view Ns.namespaces db)) (Ns.sources gs)

  -- | /O(n^2)/
  --
  -- I have no idea if it works.
  makeDiff :: (Eq a, Ord b) => (a -> b) -> [a] -> [a] -> ([a], [a], [a])
  makeDiff f xs ys = (xsys, ms, ysxs)
   where
    ms =
      map head (filter (not . null . drop 1 . List.nub)
                       (List.groupBy ((==) `on` f)
                                     (List.sortBy (compare `on` f)
                                                  (xs ++ ys))))
    xsys = List.deleteFirstsBy ((==) `on` f) xs ys
    ysxs = List.deleteFirstsBy ((==) `on` f) ys xs

red :: String
red = "\ESC[31;2m"

yellow :: String
yellow = "\ESC[33;2m"

green :: String
green = "\ESC[32;2m"

reset :: String
reset = "\ESC[0m"
