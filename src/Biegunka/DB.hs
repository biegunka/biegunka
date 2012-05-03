{-# LANGUAGE BangPatterns #-}
module Biegunka.DB
  ( load, save, merge, delete, wipe
  ) where

import Data.Functor ((<$>))
import Control.Monad (when)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import System.Directory (getHomeDirectory, doesFileExist, removeFile)
import System.FilePath ((</>))
import qualified Data.Map as M

import Biegunka.Core

load ∷ IO Biegunka
save ∷ Biegunka → IO ()
merge ∷ Biegunka → FilePath → IO Biegunka
delete ∷ Biegunka → FilePath → IO Biegunka
wipe ∷ Biegunka → IO ()

load = do
  db ← (</> ".biegunka.db") <$> getHomeDirectory
  exists ← doesFileExist db
  if exists
    then read <$> readFile db
    else return M.empty

save new = do
  !old ← load
  hd ← getHomeDirectory
  writeFile (hd </> ".biegunka.db") (show $ old <> new)

merge = undefined

delete db fp = do
  let r = M.lookup fp db
  when (isJust r) $ mapM_ removeFile (fromJust r)
  return $ M.delete fp db

wipe = undefined
