{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Pretend (pretend) where

import Control.Monad (forM_, unless, when)
import Data.Function (on)

import           Control.Monad.Free (Free)
import           Control.Monad.State (StateT, evalStateT, execState, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (getHomeDirectory)

import           Biegunka.State
import           Biegunka.DB (Biegunka(..), load)
import           Biegunka.DSL.Profile (Profile(..))
import qualified Biegunka.Interpreter.Log as Log
import qualified Biegunka.Interpreter.ConstructMap as Map


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
    ]


pretend ∷ StateT BiegunkaState (Free Profile) () → IO ()
pretend script = do
  home ← getHomeDirectory
  Biegunka α ← load
  let state = BiegunkaState { _root = home, _repositoryRoot = ""}
      script' = evalStateT script state
      β = Map.construct state script'
      stat = Stat
        { addedFiles = (countNotElems `on` files) β α
        , addedRepos = (countNotElems `on` repos) β α
        , deletedFiles = (countNotElems `on` files) α β
        , deletedRepos = (countNotElems `on` repos) α β
        }
  putStr $ show stat
  putStr $ unlines
   [ "=================="
   , ""
   , "Do you want to see full stats? [y/n]"
   ]
  c ← getChar
  when (c == 'y') $ putStr $ unlines
   [ "Added files:"
   , (logNotElems `on` files) β α
   , "Added repositories:"
   , (logNotElems `on` repos) β α
   , "Deleted files:"
   , (logNotElems `on` files) α β
   , "Deleted repositories:"
   , (logNotElems `on` repos) α β
   , "=================="
   , ""
   , "Do you want to see full log? [y/n]"
   ]
  c' ← getChar
  let installLog = Log.install state script'
      uninstallLog = Log.uninstall α β
      fullLog = installLog ++ uninstallLog
  when (c' == 'y') $ putStrLn fullLog
 where
  countNotElems xs ys = execState (ifNotElem (const $ modify succ) xs ys) 0
  logNotElems xs ys = execState (ifNotElem (\m → modify (\s → s ++ m ++ "\n")) xs ys) ""
  ifNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) (f x)
  files α = M.elems α >>= M.elems >>= S.toList
  repos α = M.elems α >>= M.keys
