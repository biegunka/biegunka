{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Interpreter.Pretend (pretend) where

import Control.Monad (forM_, unless, when)
import Data.Function (on)

import           Control.Monad.Free (Free(..))
import           Control.Monad.State (execState, modify)
import qualified Data.Map as M
import qualified Data.Set as S
import           System.Directory (getHomeDirectory)

import           Biegunka.DB (Biegunka(..), load)
import           Biegunka.Profile (Profile(..))
import qualified Biegunka.Interpreter.Pretend.Log as Log
import qualified Biegunka.Interpreter.Pretend.Map as Map


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


pretend ∷ Free (Profile a) b → IO ()
pretend script = do
  Biegunka α ← load
  home ← getHomeDirectory
  let installLog = Log.install script
      β = Map.pretend home script
      uninstallLog = Log.uninstall α β
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
   , "Do you want to see full log? [y/n]"
   ]
  c ← getChar
  when (c == 'y') $ putStrLn (installLog ++ uninstallLog)
 where
  countNotElems xs ys = execState (ifNotElem (const $ modify succ) xs ys) 0
  ifNotElem f xs ys = forM_ xs $ \x → unless (x `elem` ys) (f x)
  files α = M.elems α >>= M.elems >>= S.toList
  repos α = M.elems α >>= M.keys
