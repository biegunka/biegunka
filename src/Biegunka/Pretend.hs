{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK prune #-}
module Biegunka.Pretend (pretend) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, void, unless, when)
import Data.Function (on)

import           Control.Monad.State (execState, modify)
import qualified Data.Text.Lazy.IO as T
import           System.Directory (getHomeDirectory)
import           System.IO (hFlush, stdout)

import           Biegunka.DB (load, filepaths, sources)
import           Biegunka.Language (Script, Layer(..))
import qualified Biegunka.Log as Log
import qualified Biegunka.Map as Map
import           Biegunka.Flatten
import           Biegunka.State


data Stat = Stat
  { addedFiles ∷ Int
  , addedRepos ∷ Int
  , deletedFiles ∷ Int
  , deletedRepos ∷ Int
  } deriving (Eq, Ord)


instance Show Stat where
  show s = unlines $ map unwords
    [ [show (addedFiles s), "files", "added"]
    , [show (addedRepos s), "repositories", "added"]
    , [show (deletedFiles s), "files", "deleted"]
    , [show (deletedRepos s), "repositories", "deleted"]
    , ["------------------"]
    ]


-- | Pretend interpreter
--
-- Doesn't do any IO, so you can't check if script will fail to do IO
--
-- But Pretend can show which changes would be maid if IO will run without errors
--
-- Prints execution log if asked
--
-- @
-- main ∷ IO ()
-- main = pretend $ do
--   profile ...
--   profile ...
-- @
pretend ∷ Script Profile a → IO ()
pretend script = do
  home ← getHomeDirectory
  let script' = infect home (flatten script)
  α ← load script'
  let β = Map.construct script'
      stat = Stat
        { addedFiles = (countNotElems `on` filepaths) β α
        , addedRepos = (countNotElems `on` sources) β α
        , deletedFiles = (countNotElems `on` filepaths) α β
        , deletedRepos = (countNotElems `on` sources) α β
        }
  putStr $ show stat
  whenM ((== "y") <$> query "Do you want to see full stats?") $ putStr $ unlines
    [ "Added files:", (logNotElems `on` filepaths) β α
    , "Added repositories:", (logNotElems `on` sources) β α
    , "Deleted files:", (logNotElems `on` filepaths) α β
    , "Deleted repositories:", (logNotElems `on` sources) α β
    , "------------------"
    ]
  whenM ((== "y") <$> query "Do you want to see full log?") $
    T.putStrLn $ Log.full script' α β
  void $ putStrLn "Press any key to continue" >> getLine
 where
  countNotElems xs ys = execState (ifNotElem (const $ modify succ) xs ys) 0
  logNotElems xs ys = execState (ifNotElem (\m → modify (\s → s ++ m ++ "\n")) xs ys) ""
  ifNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) (f x)

  query s = do
    putStr (s ++ " [y/N] ") >> hFlush stdout
    getLine

  whenM ma mb = do
    p ← ma
    when p mb
