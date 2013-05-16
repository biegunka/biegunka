{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
-- | Biegunka.Source.Dummy - example Source using 'directory-layout'
module Biegunka.Source.Dummy (dummy, dummy_) where

import Control.Monad.State (state)
import System.FilePath (takeDirectory, takeFileName)
import System.Directory.Layout

import Biegunka.Language


-- | Make specified layout and attack it with 'Files'
--
-- You can do with dummy 'Source' whatever you can do with
-- real ones.but it is easier to reason about because you
-- don't depend on internet connection, installed libraries
-- and so on.
dummy :: Layout            -- ^ Layout to make
      -> FilePath          -- ^ Layout root (relative to user home directory)
      -> Script Actions () -- ^ What to do with layout files
      -> Script Sources ()
dummy l p s = do
  (ast, st) <- state $ \st -> let (ast, (succ -> st')) = annotate s st in ((ast, st'), st')
  liftS $ ES st (Source "dummy" "localhost" p updateDummy) ast ()
 where
  updateDummy dir = make (directory (takeFileName dir) l) (takeDirectory dir) >>= mapM_ print


-- | Make specified layout at given path and do nothing
--
-- This may seem as a function without any purpose but
-- it might be actually useful if you want just to
-- download 'Source' somewhere
dummy_ :: Layout   -- ^ Layout to make
       -> FilePath -- ^ Layout root (relative to user home directory)
       -> Script Sources ()
dummy_ l p = dummy l p (return ())
