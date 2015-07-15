{-# LANGUAGE DataKinds #-}
-- | Example 'Source' based on 'directory-layout'
module Control.Biegunka.Source.Layout
  ( Layout
  , layout
  ) where

import Control.Applicative (empty)
import Control.Lens
import System.FilePath (takeDirectory, takeFileName)
import System.Directory.Layout (Layout, dir, make)

import Control.Biegunka.Language
import Control.Biegunka.Script


-- | Make specified layout and use is as 'Source'
layout
  :: Layout a           -- ^ Layout to make
  -> FilePath           -- ^ Layout root
  -> Script 'Actions () -- ^ What to do with layout files
  -> Script 'Sources ()
layout dirlayout relpath = sourced Source
  { sourceType   = "dummy"
  , sourceFrom   = "localhost"
  , sourceTo     = relpath
  , sourceUpdate = update
  }
 where
  update abspath =
    return
      ( empty
      , empty <$ do
          l <- make (takeDirectory abspath) (dir (takeFileName abspath) dirlayout)
          traverseOf_ _Left print l
      )
