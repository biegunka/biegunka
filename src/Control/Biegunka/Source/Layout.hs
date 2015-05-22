{-# LANGUAGE DataKinds #-}
-- | Example 'Source' based on 'directory-layout'
module Control.Biegunka.Source.Layout (Layout, layout, layout_) where

import Control.Lens
import System.FilePath (takeDirectory, takeFileName)
import System.Directory.Layout (Layout, dir, make)

import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Make specified layout and use is as 'Source'
layout
  :: Layout a          -- ^ Layout to make
  -> FilePath          -- ^ Layout root
  -> Script 'Actions () -- ^ What to do with layout files
  -> Script 'Sources ()
layout dirlayout relpath inner = sourced "dummy" "localhost" relpath inner update
 where
  update abspath = do
    l <- make (takeDirectory abspath) (dir (takeFileName abspath) dirlayout)
    traverseOf_ _Left print l
    return Nothing


-- | Make specified layout and do nothing
layout_
  :: Layout a -- ^ Layout to make
  -> FilePath -- ^ Layout root
  -> Script 'Sources ()
layout_ l p = layout l p (return ())
