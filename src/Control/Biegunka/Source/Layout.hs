{-# LANGUAGE DataKinds #-}
-- | Example 'Source' based on 'directory-layout'
module Control.Biegunka.Source.Layout (Layout, layout, layout_) where

import System.FilePath (takeDirectory, takeFileName)
import System.Directory.Layout (Layout, directory, make)

import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Make specified layout and use is as 'Source'
layout
  :: Layout            -- ^ Layout to make
  -> FilePath         -- ^ Layout root
  -> Script Actions () -- ^ What to do with layout files
  -> Script Sources ()
layout dirlayout relpath inner = sourced "dummy" "localhost" relpath inner update
 where
  update abspath = do
    errors <- make (directory (takeFileName abspath) dirlayout) (takeDirectory abspath)
    mapM_ print errors


-- | Make specified layout and do nothing
layout_
  :: Layout   -- ^ Layout to make
  -> FilePath -- ^ Layout root
  -> Script Sources ()
layout_ l p = layout l p (return ())
