{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Pretend (pretend) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, void, unless, when)
import Data.Function (on)

import           Control.Monad.State (execState, modify)
import           Data.Default (Default)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (getHomeDirectory)
import           System.IO (hFlush, stdout)

import           Biegunka.DB (Biegunka(..), load)
import           Biegunka.DSL (ProfileScript)
import qualified Biegunka.Interpreter.Common.Log as Log
import qualified Biegunka.Interpreter.Common.Map as Map
import           Biegunka.Interpreter.Common.State


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


pretend ∷ Default s ⇒ ProfileScript s () → IO ()
pretend script = do
  home ← getHomeDirectory
  Biegunka α ← load
  let script' = infect home script
      β = Map.construct script'
      stat = Stat
        { addedFiles = (countNotElems `on` files) β α
        , addedRepos = (countNotElems `on` repos) β α
        , deletedFiles = (countNotElems `on` files) α β
        , deletedRepos = (countNotElems `on` repos) α β
        }
  putStr $ show stat
  whenM ((== "y") <$> query "Do you want to see full stats?") $ putStr $ unlines
    [ "Added files:", (logNotElems `on` files) β α
    , "Added repositories:", (logNotElems `on` repos) β α
    , "Deleted files:", (logNotElems `on` files) α β
    , "Deleted repositories:", (logNotElems `on` repos) α β
    , "------------------"
    ]
  whenM ((== "y") <$> query "Do you want to see full log?") $
    putStrLn $ Log.install script' ++ Log.uninstall α β
  void $ putStrLn "Press any key to continue" >> getLine
 where
  countNotElems xs ys = execState (ifNotElem (const $ modify succ) xs ys) 0

  logNotElems xs ys = execState (ifNotElem (\m → modify (\s → s ++ m ++ "\n")) xs ys) ""

  ifNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) (f x)

  files α = M.elems α >>= M.elems >>= S.toList

  repos α = M.elems α >>= M.keys

  query s = do
    putStr (s ++ " [y/N] ") >> hFlush stdout
    getLine

  whenM ma mb = do
    p ← ma
    when p mb
