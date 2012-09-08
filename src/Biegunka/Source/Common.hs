{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Source.Common where

import Control.Applicative ((<$>))
import Control.Monad (when)

import Data.ByteString.Lazy (ByteString)
import Network.Curl.Download.Lazy (openLazyURI)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)


remove ∷ FilePath → IO ()
remove path =
  mapM_ (uncurry whenM) [(doesDirectoryExist, removeDirectoryRecursive), (doesFileExist, removeFile)]
 where
  whenM predicateM remove' = predicateM path >>= flip when (remove' path)


download ∷ String → IO ByteString
download url = either (error . show) id <$> openLazyURI url
