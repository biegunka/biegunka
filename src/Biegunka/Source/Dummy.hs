{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
-- | Biegunka.Source.Dummy - example Source using 'directory-layout'
module Biegunka.Source.Dummy (dummy, dummy_, sourceFailure) where

import Control.Exception.Lifted (throwIO)
import Control.Monad ((<=<))

import Control.Monad.Free (liftF)
import Data.Text (Text)
import System.Directory.Layout (Layout, make)

import Biegunka.Language (Script, Layer(Files, Source), Command(S))
import Biegunka.Execute (BiegunkaException(SourceEmergingFailure))


-- | Make specified layout and attack it with 'Files'
--
-- You can do with dummy 'Source' whatever you can do with
-- real ones.but it is easier to reason about because you
-- don't depend on internet connection, installed libraries
-- and so on.
dummy :: Layout          -- ^ Layout to make
      -> FilePath        -- ^ Layout root (relative to user home directory)
      -> Script Files () -- ^ What to do with layout files
      -> Script Source ()
dummy l p s = liftF $ S "localhost" p s (mapM_ print <=< make l) ()


-- | Make specified layout at given path and do nothing
--
-- This may seem as a function without any purpose but
-- it might be actually useful if you want just to
-- download 'Source' somewhere
dummy_ :: Layout   -- ^ Layout to make
       -> FilePath -- ^ Layout root (relative to user home directory)
       -> Script Source ()
dummy_ l p = dummy l p (return ())


-- | Report 'Source' emerge failure to Biegunka.
sourceFailure :: String -> FilePath → Text → IO a
sourceFailure up fp fs = throwIO $ SourceEmergingFailure up fp fs
