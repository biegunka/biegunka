{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Pretend (pretend) where

import Data.List ((\\))
import Control.Monad (when)

import           Control.Lens
import qualified Data.Text.Lazy.IO as T
import           System.IO

import           Biegunka.DB
import qualified Biegunka.Log as Log
import           Biegunka.Control (Interpreter(..), root)


-- | Pretend interpreter
--
-- Doesn't do any IO, so you can't check if script will fail to do IO
--
-- But Pretend can show which changes would be maid if IO will run without errors
--
-- Prints execution log if asked
--
-- @
-- main :: IO ()
-- main = pretend $ do
--   profile ...
--   profile ...
-- @
pretend :: Interpreter
pretend = I $ \c s -> do
  a <- load (c ^. root) s
  let b = construct s
  putStr . talk $ stats a b
  whenM (query "Print full log?") $
    T.putStrLn $ Log.full s a b
 where
  whenM ma mb = do
    p <- ma
    when p mb


data Stats = Stats
  { addedF, addedS, deletedF, deletedS :: [FilePath]
  } deriving (Show, Read, Eq, Ord)


stats :: Biegunka -> Biegunka -> Stats
stats a b = Stats
  { addedF   = filepaths b \\ filepaths a
  , addedS   = sources b   \\ sources a
  , deletedF = filepaths a \\ filepaths b
  , deletedS = sources a   \\ sources b
  }


talk :: Stats -> String
talk (Stats af as df ds) = concat
  [ about "added files" af
  , about "added sources" as
  , about "deleted files" df
  , about "deleted sources" ds
  ]
 where
  about msg xs = let c = length xs in case c of
    0 -> ""
    _ -> msg ++ " (" ++ show c ++ "):\n" ++ unlines (map ("  " ++) xs)


query :: String -> IO Bool
query s = do
  putStr (s ++ " [yN] ")
  hFlush stdout
  c <- getChar'
  putStrLn ""
  return (c == 'y')


getChar' :: IO Char
getChar' = do
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin LineBuffering
  return c
