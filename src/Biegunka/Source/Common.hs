{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_HADDOCK hide #-}
module Biegunka.Source.Common where

import Control.Applicative ((<$>))
import Control.Monad (when)

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Digest.Pure.SHA (bytestringDigest, sha1)
import           Network.Curl.Download.Lazy (openLazyURI)
import           System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import           System.FilePath ((</>))


download ∷ String → IO ByteString
download url = either (error . show) id <$> openLazyURI url


remove ∷ FilePath → IO ()
remove path =
  mapM_ (uncurry whenM) [(doesDirectoryExist, removeDirectoryRecursive), (doesFileExist, removeFile)]
 where
  whenM predicateM remove' = predicateM path >>= flip when (remove' path)


update ∷ String → FilePath → (ByteString → IO ()) → IO ()
update url path f = do
  archive ← download url
  let newHash = hash archive
      unpack = remove path >> f archive >> writeHash path newHash
  e ← doesDirectoryExist path
  if e
    then maybe unpack (\oldHash → when (newHash /= oldHash) unpack) =<< readHash path
    else unpack


hash ∷ ByteString → ByteString
hash = bytestringDigest . sha1


readHash ∷ FilePath → IO (Maybe ByteString)
readHash path = do
  let file = path </> ".biegunka.hash"
  e ← doesFileExist file
  if e
    then Just <$> B.readFile file
    else return Nothing


writeHash ∷ FilePath → ByteString → IO ()
writeHash path = B.writeFile (path </> ".biegunka.hash")
